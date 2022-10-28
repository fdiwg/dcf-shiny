plugin_template_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    tags$div("THIS IS PLUGIN TEMPLATE")
  )
  
}