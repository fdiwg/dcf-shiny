function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    uiOutput(ns("wizard"))
  )
  
}