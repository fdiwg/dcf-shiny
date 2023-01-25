data_user_submissions_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_user_submissions",
    h3("My submissions"),hr(),
    fluidRow(
      div(
        class = "col-md-2",
        uiOutput(ns("task_wrapper")),
        uiOutput(ns("datacall_wrapper")),
        uiOutput(ns("run_wrapper"))
      ),
      div(
        class = "col-md-10",
        uiOutput(ns("indicators_wrapper"))
      )
    ),
    uiOutput(ns("refresh_wrapper")),
    uiOutput(ns("table_wrapper"))
  )
}