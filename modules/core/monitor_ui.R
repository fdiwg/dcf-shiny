monitor_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "monitor",
    DT::dataTableOutput(ns("resources"))
  )
}