monitor_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "monitor",
    uiOutput(ns("machine_resources_wrapper")),
    uiOutput(ns("software_resources_wrapper")),
    uiOutput(ns("token_wrapper"))
  )
}