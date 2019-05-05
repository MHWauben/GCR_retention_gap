ui <- dashboardPage(
  dashboardHeader(title = "GCR retention gap"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "Headline",
      tabName = "headline",
      icon = icon("dashboard")),
    menuItem(
      "Detail", 
      tabName = "detail", 
      icon = icon("th"))
  )),
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
                          label = "Year policy implemented",
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
                    h2("Administrative department"))
              ),
        fluidRow(
          box(title = "Joining rate",
              status = "info",
              width = 4,
              plotlyOutput('join_adm')
          ),
          box(title = "Leaving rate",
              status = "info",
              width = 4,
              plotlyOutput('leave_adm')
          ),
          box(title = "Termination rate",
              status = "warning",
              width = 4,
              plotlyOutput('term_adm')
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
              plotlyOutput('join_op')
          ),
          box(title = "Leaving rate",
              status = "info",
              width = 4,
              plotlyOutput('leave_op')
          ),
          box(title = "Termination rate - workforce",
              status = "warning",
              width = 4,
              plotlyOutput('term_op')
          )
        
        )
      )
    )
  )
)