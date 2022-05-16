data_validation2_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_validation2",
    uiOutput(ns("wizard"))
  )
}