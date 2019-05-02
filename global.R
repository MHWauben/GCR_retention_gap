# Load information pre-launch

library(shiny)
library(plotly)
library(shinydashboard)
library(dplyr)
library(tidyr)


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


# People who have left: ----
data_left <- data[!data$current, ]



