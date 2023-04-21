monitor_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "monitor",
    uiOutput(ns("ressources_wrapper")),
    uiOutput(ns("token_wrapper"))
  )
}