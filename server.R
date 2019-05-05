function(input, output) { 
  
  # Conditional forecast -----
  year_filter <- reactive({
    input$year_filter
  })
  
  output$headline <- renderUI({
    tags$h2(paste0("If women had been fired at the same rate as men since ", 
                   year_filter()))
  })
  
  output$explanation <- renderUI({
    HTML(paste0("As a proportion of the population, women and men join and leave at the same rate. ",
           "However, at GCR women are much more likely to be terminated than men. ",
           "Termination is a human decision: this difference indicates some gender bias in the termination process. ",
           "This bias can be reduced through implementing behavioural insights interventions. ",
           "<p/> ",
           "What if the termination rate for women was the same as for men? ",
           "<p/> ",
           "Use the drop-down below to select the year at which you could have changed the termination process to be less biased. ",
           "The graph will then show how many more women you would have retained by this year. "))
  })
  
  data_freq <- reactive({
    req(input$year_filter)
    data_freq <- rbind(workforce_trend %>% mutate(qrt = "1"),
                       workforce_trend %>% mutate(qrt = "3")) %>%
      dplyr::filter(year > year_filter())
    return(data_freq)
    print("data_freq done")
  })
  
  m_trend <- reactive({
    m_trend <- ts(data_freq()[data_freq()$gender == "M", ]$term_rate, frequency = 2) %>%
      decompose(type = "multiplicative")
    return(m_trend)
    print("m_trend done")
  })
  
  f_terms <- reactive({
    f_terms <- ts(data_freq()[data_freq()$gender == "M", ]$term_rate, frequency = 2) %>%
      decompose(type = "multiplicative")
    return(f_terms)
    print("f_terms done")
  })
  
  f_f_forecast <- reactive({
    f_f_forecast <- f_terms()$seasonal * f_terms()$random * f_terms()$trend
    return(f_f_forecast)
  })
  
  f_m_forecast <- reactive({
    f_m_forecast <- f_terms()$seasonal * f_terms()$random * m_trend()$trend
    return(f_m_forecast)
  })
  
  workforce_WIP <- reactive({
    workforce_WIP <- data_freq() %>%
      dplyr::filter(gender == "F") %>%
      dplyr::mutate(f_m_forecast = c(as.vector(f_m_forecast()[2:length(f_m_forecast())]), 2),
                    workforce = total) %>%
      dplyr::mutate(forecast_term = total * f_m_forecast) %>%
      dplyr::mutate(prev_term = lag(forecast_term, 1)) %>%
      dplyr::mutate(change = new - resigned - retired - prev_term) %>%
      dplyr::select(year, gender, workforce, change) %>%
      setDT(.)
    
    for (row in 3:nrow(workforce_WIP)){
      new_workforce <- workforce_WIP[row - 1L, workforce + change]
      workforce_WIP[row, workforce := new_workforce]
    }
    
    return(workforce_WIP)
  })
    
  workforce_freq_new <- reactive({
    workforce_freq_new <- copy(workforce_WIP()) %>%
      .[, .(forecast_workforce = mean(workforce)), 
        by = "year"] %>%
      rbind(., workforce_trend %>% 
              filter(year <= year_filter() & gender == "F") %>% 
              ungroup () %>% 
              select(year, forecast_workforce = total))
    
    return(workforce_freq_new)
  })
  
  workforce_forecast <- reactive({
    workforce_forecast <- data_freq() %>%
    dplyr::select(-qrt) %>%
    unique(.) %>%
    dplyr::filter(gender == "F") %>%
    dplyr::ungroup() %>%
    dplyr::select(year, total) %>%
    rbind(., workforce_trend %>% 
            filter(year <= year_filter() & gender == "F") %>% 
            ungroup () %>% 
            select(year, total)) %>%
    merge(., workforce_freq_new(), by = "year") %>%
    dplyr::select(year, 
                  "Actual" = total, 
                  "Forecast" = forecast_workforce) %>%
    tidyr::gather(key, value, -year)
    return(workforce_forecast)
  })
  
  difference <- reactive({
    difference <- workforce_forecast() %>%
      tidyr::spread(key, value) %>%
      dplyr::mutate(diff = round(Forecast - Actual, 0),
                    Forecast = round(Forecast, 0),
                    Actual = round(Actual, 0)) %>%
      dplyr::filter(year > as.numeric(year_filter()) - 5) %>%
      dplyr::arrange(year)
    return(difference)
  })
  
  output$difference <- renderValueBox({
    valueBox(
      value = difference()[nrow(difference()), "diff"],
      subtitle = "More women by now",
      icon = icon("credit-card"),
      color = "aqua"
    )
  })
  
  # output$forecast_plot <- renderPlot({
  #   ggplot(workforce_forecast()[workforce_forecast()$year > (as.numeric(year_filter())-5), ], 
  #          aes(x = year, y = value, colour = reorder(key, desc(key))))+
  #     geom_line()+
  #     geom_line(data = workforce_forecast()[workforce_forecast()$year <= year_filter() & 
  #                                          workforce_forecast()$year > (as.numeric(year_filter())-5), ])+
  #     # geom_smooth(data = workforce_forecast()[workforce_forecast()$year > (year_filter() - 1), ], 
  #     #             se = FALSE)+
  #     labs(x = "Year",
  #          y = "Number of women in the workforce",
  #          colour = " ")+
  #     BIT_theme+
  #     scale_colour_manual(values = c(strong_palette[2], strong_palette[1]))
  # })
  
  headline_plot <- function(year, var1, var2, name) {
    plot_ly(x = ~year, y = ~var1, 
            name = 'Forecast',
            type = 'scatter', mode = 'lines', 
            line = list(shape = "spline",
                        color = 'rgb(255, 132, 0)'),
            showlegend = TRUE,
            hoverinfo = "all") %>%
      add_trace(y = ~var2, 
                name = 'Actual',
                type = 'scatter', mode = 'lines', 
                line = list(shape = "spline",
                            color = "rgb(0, 209, 252)"),
                showlegend = TRUE,
                hoverinfo="all") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list (title = "Number of women in the workforce"),
             legend = list(x = 100,
                           y = 0.5))
  }
  
  output$forecast <- renderPlotly({
    headline_plot(difference()$year,
                  difference()$Forecast,
                  difference()$Actual,
                  " ")
  })
  
  
  # Rate plots -----
  
  data_rate <- data_timeline %>%
    dplyr::group_by(year,  gend_dept) %>%
    dplyr::summarise(workforce = n(),
                     left = sum(current != TRUE),
                     joined = sum(new == TRUE),
                     terminated = sum(term, na.rm = T)) %>%
    dplyr::mutate(joining_rate = round(joined / workforce, 3),
                  leaving_rate = round(left / workforce, 3),
                  termination_rate = round(terminated / workforce, 3),
                  term_leaving_rate = round(terminated / left, 3)) %>%
    dplyr::select(year, gend_dept, joining_rate, 
                  leaving_rate, termination_rate, term_leaving_rate) %>%
    tidyr::gather(variable, value, -(year:gend_dept)) %>%
    dplyr::mutate(rate = paste0(gend_dept, variable),
                  gend_dept = NULL,
                  variable = NULL) %>%
    tidyr::spread(key = rate, value = value)
  
  two_lines <- function(year, var1, var2, name) {
    plot_ly(x = ~year, y = ~var1, 
            type = 'scatter', mode = 'lines', 
            line = list(alpha = 0.5,
                        shape = "spline",
                        color = 'rgba(255, 132, 0, 0.25)'),
            showlegend = FALSE,
            hoverinfo = "all") %>%
      add_lines(y = ~fitted(loess(var1~year)),
                name = paste0("Female "), 
                line = list(color = 'rgba(255, 132, 0, 1)'),
                showlegend = TRUE,
                hoverinfo="none") %>%
      add_trace(y = ~var2, 
                type = 'scatter', mode = 'lines', 
                line = list(alpha = 0.5,
                            shape = "spline",
                            color = "rgba(0, 209, 252, 0.25)"),
                showlegend = FALSE,
                hoverinfo="all") %>%
      add_lines(y = ~fitted(loess(var2~year)),
                name = paste0("Male "),
                line = list(color = "rgba(0, 209, 252, 1)"),
                showlegend = TRUE,
                hoverinfo="false") %>%
      layout(xaxis = list(title = " "),
             yaxis = list (title = "Rate"),
             legend = list(orientation = 'h', 
                           xanchor = "center",  
                           x = 0.5,
                           y = -0.2))
  }
  
  output$join_adm <- renderPlotly({
    two_lines(data_rate$year, 
                     data_rate$F_Administrativejoining_rate, 
                     data_rate$M_Administrativejoining_rate,
                     "administrative joining rate")
  })
  
  output$leave_adm <- renderPlotly({
    two_lines(data_rate$year, 
                           data_rate$F_Administrativeleaving_rate, 
                           data_rate$M_Administrativeleaving_rate,
                           "administrative joining rate")
  })
  
  output$term_adm <- renderPlotly({
    two_lines(data_rate$year,
              data_rate$F_Administrativetermination_rate,
              data_rate$M_Administrativetermination_rate,
              "administrative termination rate out of the workforce")
  })
  
  # output$term_leave_adm <- renderPlotly({
  #   two_lines(data_rate$year,
  #             data_rate$F_Administrativeterm_leaving_rate,
  #             data_rate$M_Administrativeterm_leaving_rate,
  #             "administrative termination rate out of the leavers")
  # })
  
  output$join_op <- renderPlotly({
    two_lines(data_rate$year, 
              data_rate$F_Operationaljoining_rate, 
              data_rate$M_Operationaljoining_rate,
              "operational joining rate")
  })
  
  output$leave_op <- renderPlotly({
    two_lines(data_rate$year, 
              data_rate$F_Operationalleaving_rate, 
              data_rate$M_Operationalleaving_rate,
              "operational joining rate")
  })
  
  output$term_op <- renderPlotly({
    two_lines(data_rate$year,
              data_rate$F_Operationaltermination_rate,
              data_rate$M_Operationaltermination_rate,
              "operational termination rate out of the workforce")
  })
  
  # output$term_leave_op <- renderPlotly({
  #   two_lines(data_rate$year,
  #             data_rate$F_Operationalterm_leaving_rate,
  #             data_rate$M_Operationalterm_leaving_rate,
  #             "operational termination rate out of the leavers")
  # })
  
  
  
  }
