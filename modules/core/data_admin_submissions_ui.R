data_admin_submissions_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_admin_submissions",
    h3("All submissions"),hr(),
    fluidRow(
      div(
        class = "col-md-3",
        uiOutput(ns("task_wrapper")),
        uiOutput(ns("datacall_wrapper"))
      ),
      div(
        class = "col-md-3",
        uiOutput(ns("percent"))
      ),
      div(
        class = "col-md-4",
        uiOutput(ns("indicators"))
      )
    ),
    uiOutput(ns("refresh_wrapper")),
    uiOutput(ns("table_wrapper"))
  )
}