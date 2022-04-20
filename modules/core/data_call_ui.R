data_call_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_calls",
    shiny::actionButton(inputId = ns("add_data_call"), label = "Add a new data call", class = "btn-primary"),
    DT::DTOutput(ns("tbl_data_calls"))
  )
}