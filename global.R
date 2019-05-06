# Load information pre-launch

library(shiny)
library(plotly)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(data.table)
library(broom)
library(DT)


### Save theme colours -----
strong_palette <- c("#00aae6", "#ff8400", "#00d1fc")
soft_palette <- c("#dcf4fe", "#fff0df")


### Loading and calculating data ------
data <- read.csv('GCR.csv') %>%
  dplyr::mutate(gender = factor(gender, levels = c("M", "F")),
                gradeAtHire = as.ordered(gradeAtHire),
                age = ageAtHire + tenure,
                gend_dept = paste(gender, department, sep = "_"),
                leaveYear = hireYear + tenure,
                term = ifelse(reason == "terminated", 1, 0))
# str(data)
summary(data)

# Workforce over time: ----
all_years <- expand.grid(year = unique(c(data$hireYear, data$leaveYear)),
                         id = unique(data$id))
data_timeline <- dplyr::full_join(data, all_years, by = "id") %>%
  dplyr::filter(hireYear <= year & leaveYear >= year) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(tenure = year - hireYear, 
                reason = ifelse(year != max(year), NA, as.character(reason)),
                current = ifelse(is.na(reason), TRUE, FALSE),
                term = ifelse(year != max(year), NA, as.numeric(term)),
                new = hireYear == year)

# Workforce over time - rate of termination -----
workforce_trend <- data_timeline %>%
  dplyr::group_by(year, gender, reason) %>%
  dplyr::summarise(number = n()) %>%
  merge(., yeargenderreason, all.x = T, all.y = T, by = c("year", "gender", "reason")) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number),
                reason = ifelse(is.na(reason), "stayed", reason)) %>%
  tidyr::spread(reason, number) %>%
  merge(., new_year, all.x = T, all.y = T, by = c("year", "gender")) %>%
  dplyr::mutate(total = stayed + retired + resigned + terminated) %>%
  dplyr::mutate(join_rate = new / total,
                term_rate = terminated / total,
                nonterm_rate = (retired + resigned) / total) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(gender) %>%
  dplyr::mutate(join_avg = zoo::rollmean(join_rate, 5, na.pad = T, align = "right"),
                term_avg = zoo::rollmean(term_rate, 5, na.pad = T, align = "right"),
                nonterm_avg = zoo::rollmean(nonterm_rate, 5, na.pad = T, align = "right"))


# People who have left: ----
data_left <- data_timeline %>%
  dplyr::filter(!is.na(reason))



# Calculate rates: ----
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



# Plotting functions: ----

# Headline forecast:
headline_plot <- function(year, var1, var2, name) {
  plot_ly(x = ~year, y = ~var1, 
          name = 'Forecast',
          type = 'scatter', mode = 'lines', 
          line = list(shape = "spline",
                      color = 'rgb(255, 132, 0)'),
          showlegend = TRUE,
          hoverinfo = "y") %>%
    add_trace(y = ~var2, 
              name = 'Actual',
              type = 'scatter', mode = 'lines', 
              line = list(shape = "spline",
                          color = "rgb(0, 209, 252)"),
              showlegend = TRUE,
              hoverinfo="y") %>%
    layout(xaxis = list(title = "Year"),
           yaxis = list (title = "Number of women in the workforce"),
           legend = list(x = 100,
                         y = 0.5),
           hovermode = 'compare')
}

# Comparing rates between genders:
two_lines <- function(year, var1, var2, name) {
  plot_ly(x = ~year, y = ~var1, 
          type = 'scatter', mode = 'lines', 
          line = list(alpha = 0.5,
                      shape = "spline",
                      color = 'rgba(255, 132, 0, 0.25)'),
          showlegend = FALSE,
          hoverinfo = "y") %>%
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
              hoverinfo="y") %>%
    add_lines(y = ~fitted(loess(var2~year)),
              name = paste0("Male "),
              line = list(color = "rgba(0, 209, 252, 1)"),
              showlegend = TRUE,
              hoverinfo="none") %>%
    layout(xaxis = list(title = " "),
           yaxis = list (title = "Rate"),
           legend = list(orientation = 'h', 
                         xanchor = "center",  
                         x = 0.5,
                         y = -0.2))
}
