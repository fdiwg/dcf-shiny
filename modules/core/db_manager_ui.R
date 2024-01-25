db_manager_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    uiOutput(ns("wizard"))
  )
  
}