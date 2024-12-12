monitor_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "monitor",
    uiOutput(ns("resources_wrapper"))
  )
}