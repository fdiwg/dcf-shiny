user_management_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "user_management",
    shiny::uiOutput(ns("user_table"))
  )
}