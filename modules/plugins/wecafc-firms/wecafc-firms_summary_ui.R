function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    h3("Data availability"),
    uiOutput(ns("menu"))
  )
  
}