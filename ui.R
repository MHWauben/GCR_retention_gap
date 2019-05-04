ui <- dashboardPage(
  dashboardHeader(),
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
    dateInput(inputId = "year_filter", 
              label = "Year policy implemented", 
              value = '2010-01-01', 
              min = '2005-01-01', 
              max = '2017-12-31',
              format = "yyyy", 
              startview = "year", 
              weekstart = 0,
              language = "en", width = NULL)
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "headline",
        fluidRow(
          box(title = "",
              status = "primary",
              width = 12,
              plotOutput('forecast_plot'))
        )
        ),
      tabItem(tabName = "detail",
        h2("Administrative department"),
        fluidRow(
          box(title = "Joining rate",
              status = "primary",
              width = 3,
              plotlyOutput('join_adm')
          ),
          box(title = "Leaving rate",
              status = "primary",
              width = 3,
              plotlyOutput('leave_adm')
          ),
          box(title = "Termination rate - workforce",
              status = "warning",
              width = 3,
              plotlyOutput('term_adm')
          ),
          box(title = "Termination rate - leavers",
              status = "warning",
              width = 3,
              plotlyOutput('term_leave_adm'))
        ),
        h2("Operational department"),
        fluidRow(
          box(title = "Joining rate",
              status = "primary",
              width = 3,
              plotlyOutput('join_op')
          ),
          box(title = "Leaving rate",
              status = "primary",
              width = 3,
              plotlyOutput('leave_op')
          ),
          box(title = "Termination rate - workforce",
              status = "warning",
              width = 3,
              plotlyOutput('term_op')
          ),
          box(title = "Termination rate - leavers",
              status = "warning",
              width = 3,
              plotlyOutput('term_leave_op'))
        
        )
      )
    )
  )
)