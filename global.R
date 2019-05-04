# Load information pre-launch

library(shiny)
library(plotly)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(data.table)


### Set theme -----
strong_palette <- c("#00aae6", "#ff8400", "#00d1fc")
soft_palette <- c("#dcf4fe", "#fff0df")

BIT_theme <- theme_minimal()+
  theme(plot.title = element_text(color = "#00aae6",
                                  size = rel(1.8)),
        plot.subtitle = element_text(color = "#00aae6"),
        strip.text.x = element_text(size = rel(1.1), 
                                    colour = "black",
                                    margin = margin(t = 2,
                                                    b = 2)),
        strip.background = element_rect(fill = "#dcf4fe", colour = NA))



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
  dplyr::mutate(reason = ifelse(year != max(year), NA, as.character(reason)),
                current = ifelse(year != max(year), TRUE, FALSE),
                term = ifelse(year != max(year), NA, as.numeric(term)),
                new = hireYear == year)

# Proportion of women in the workforce any year: ----
workforce_prop_timeline <- data_timeline %>%
  dplyr::group_by(year, gender) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(prop_workforce = n / sum(n)) %>%
  dplyr::filter(gender == "F") %>%
  dplyr::select(year, prop_workforce)

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
data_left <- data[!data$current, ]



