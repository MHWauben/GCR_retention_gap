ui <- dashboardPage(
  dashboardHeader(title = "GCR retention gap",
                  tags$li(a(href = 'https://github.com/MHWauben/GCR_retention_gap',
                            icon("github"),
                            title = "To code repository"),
                          class = "dropdown")),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "Headline",
      tabName = "headline",
      icon = icon("dashboard")),
    menuItem(
      "Detail", 
      tabName = "detail", 
      icon = icon("th")),
    menuItem(
      "Statistics",
      tabName = "stats",
      icon = icon("tasks"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("h2 {font-weight: 500;
                            line-height: 1.1;
                            color: #00aae6;}"))
      ),
    tabItems(
      tabItem(
        tabName = "headline",
        fluidRow(
          box(width = 12,
              h2("If women had been fired at the same rate as men..."),
              htmlOutput('explanation'))
        ),
        fluidRow(
          box(width = 8,
              status = "info",
              selectInput(inputId = "year_filter",
                          label = "Year termination policy became less biased",
                          choices = c(2005:(max(data$hireYear) - 3)),
                          selected = 2010)
          ),
          valueBoxOutput('difference')
        ),
        fluidRow(
          column(width = 12, 
                 plotlyOutput('forecast')
                 )
        )
        ),
      tabItem(tabName = "detail",
              fluidRow(
                box(width = 12,
                    h2("Joining, leaving and termination rates"),
                    htmlOutput("breakdown"))
              ),
              fluidRow(
                box(width = 12,
                    h2("Administrative department"))
              ),
        fluidRow(
          box(title = "Joining rate",
              status = "info",
              width = 4,
              plotlyOutput('join_adm',
                           height = '250px')
          ),
          box(title = "Leaving rate",
              status = "info",
              width = 4,
              plotlyOutput('leave_adm',
                           height = '250px')
          ),
          box(title = "Termination rate",
              status = "warning",
              width = 4,
              plotlyOutput('term_adm',
                           height = '250px')
          )
        ),
        fluidRow(
          box(width = 12, 
              h2("Operational department"))
        ),
        fluidRow(
          box(title = "Joining rate",
              status = "info",
              width = 4,
              plotlyOutput('join_op',
                           height = '250px')
          ),
          box(title = "Leaving rate",
              status = "info",
              width = 4,
              plotlyOutput('leave_op',
                           height = '250px')
          ),
          box(title = "Termination rate - workforce",
              status = "warning",
              width = 4,
              plotlyOutput('term_op',
                           height = '250px')
          )
        
        )
      ),
      tabItem(tabName = "stats",
              fluidRow(
                box(width = 12,
                    h2("Further statistical exploration"),
                    htmlOutput('stats_explanation')
                )
              ),
              fluidRow(
                box(width = 6,
                    status = "info",
                    checkboxGroupInput("predictors", "Choose the factors",
                                   selected = list("department", "gender"),
                                   choiceNames = list("Department",
                                                      "Gender",
                                                      "Grade at Hire", 
                                                      "Age at Hire",
                                                      "Hiring Year",
                                                      "Leaving Year",
                                                      "Tenure"),
                                   choiceValues = list("department", 
                                                       "gender", 
                                                       "gradeAtHire",
                                                       "ageAtHire",
                                                       "hireYear",
                                                       "leaveYear",
                                                       "tenure"),
                                   inline = TRUE)
                ),
                box(width = 3,
                    status = "info",
                    numericInput(inputId = "year_min",
                                label = "First year for the analysis",
                                value = min(data_timeline$year),
                                min = min(data_timeline$year),
                                max = max(data_timeline$year) - 1)
                ),
                box(width = 3, 
                    status = "info",
                    numericInput(inputId = "year_max",
                                 label = "Last year for the analysis",
                                 value = max(data_timeline$year),
                                 min = min(data_timeline$year) + 1,
                                 max = max(data_timeline$year))
                )
              ),
              fluidRow(
                box(width = 12,
                    dataTableOutput('aovSummary')
              ))
      )
    )
  )
)
