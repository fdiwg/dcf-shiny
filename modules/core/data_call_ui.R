data_call_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_call",
    shiny::actionButton(inputId = ns("add_data_call"), label = "Add a new data call", class = "btn-primary"),
    br(),br(),
    fluidRow(
      div(
        class = "col-md-2",
        uiOutput(ns("task_selector"))
      ),
      div(
        class = "col-md-2",
        uiOutput(ns("status_selector"))
      )
    ),
    withSpinner(DT::DTOutput(ns("tbl_data_calls")), type = 4)
  )
}