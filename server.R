function(input, output) { 
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
  
  output$term_leave_adm <- renderPlotly({
    two_lines(data_rate$year,
              data_rate$F_Administrativeterm_leaving_rate,
              data_rate$M_Administrativeterm_leaving_rate,
              "administrative termination rate out of the leavers")
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
  
  output$term_leave_op <- renderPlotly({
    two_lines(data_rate$year,
              data_rate$F_Operationalterm_leaving_rate,
              data_rate$M_Operationalterm_leaving_rate,
              "operational termination rate out of the leavers")
  })
  
  
  
  }
