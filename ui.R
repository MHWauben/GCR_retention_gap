ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
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