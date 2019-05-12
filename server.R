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
    HTML(paste0("As a proportion of the population of that gender, women and men join and leave at the same rate. ",
           "However, at GCR women are much more likely to be terminated than men. ",
           "To find out more, look at the 'Detail' and 'Statistics' tabs. ",
           "<p/> ",
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
    m_trend <- ts(data_freq()[data_freq()$gender == "M", ]$term_rate, 
                  frequency = 2) %>%
      decompose(type = "multiplicative")
    return(m_trend)
    print("m_trend done")
  })
  
  f_terms <- reactive({
    f_terms <- ts(data_freq()[data_freq()$gender == "M", ]$term_rate, 
                  frequency = 2) %>%
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
  
  output$forecast <- renderPlotly({
    headline_plot(difference()$year,
                  difference()$Forecast,
                  difference()$Actual,
                  " ")
  })
  
  
  # Rate plots -----
  
  output$breakdown <- renderUI({
    HTML(paste0("This page show the joining, leaving, and termination rate broken down by department and gender. ",
                "Each rate is calculated over the workforce in that category. ",
                "For example, the Female joining rate in the Administrative department is the number of new women in that department divided by the total number of women in that department. ",
                "<p/> ",
                "This allows you to see clearly that the rate of termination is much higher in the female workforce, particularly in the administrative department. "))
  })
  
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
  
  
  # Statistical association between gender and termination ----
  
  output$stats_explanation <- renderUI({
    HTML(paste0("This page may be used by GCR internal statisticians to establish the relationship between the termination rate and factors other than gender. ",
                "You can select any number of predictors below. ",
                "You can also select which years of data to include. ",
                "The table will show you the summary table of an ANOVA test, highlighting any p-values below 0.05. ",
                "<p/> ",
                "<b>NOTE:</b> this analysis only includes leavers. ", 
                "Therefore, this analysis explores the relationship between these predictors and the reason for leaving, not which individuals leave in the first place. ",
                "To explore this relationship, use the graphs under the Detail tab. "))
  })
  
  aov_data <- reactive({
    aov_data <- data_left %>%
      dplyr::filter(year >= input$year_min & year <= input$year_max)
    return(aov_data)
  })
  
  anova_formula <- reactive({
    as.formula(paste0("term ~ ", paste(input$predictors, collapse = " * ")))
  })
  
  aovSummary <- reactive({
    aovSummary <- broom::tidy(anova(aov(anova_formula(), data = aov_data()))) %>%
      dplyr::mutate(sumsq = round(sumsq, 2),
                    statistic = round(statistic, 2),
                    p.value = round(p.value, 3),
                    sign = ifelse(p.value < 0.05,
                                  "Significant",
                                  "")) %>%
      dplyr::select(Term = term,
                    "Degrees of Freedom" = df,
                    "Sum of Squares" = sumsq,
                    "Statistic" = statistic,
                    "p value" = p.value,
                    "Significant?" = sign)
  })
  
  output$aovSummary <- renderDataTable({
    DT::datatable(aovSummary(),
                  options = list(pageLength = nrow(aovSummary()))) %>% 
      formatStyle('Significant?', target = "row", 
                  backgroundColor = styleEqual(c('Significant'), c("#ff8400")))
  })
  
  }
