data_entry_editor_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_entry_editor",
    h3("Edit data"),hr(),
    div(
      uiOutput(ns("menu"))
    )
  )
}