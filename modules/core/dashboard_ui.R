dashboard_ui <- function(id){
  
  ns <- NS(id)
 
  shinydashboard::tabItem(
    tabName = "dashboard",
    h4("Username -->"),
    verbatimTextOutput(ns("SHINYPROXY_USERNAME")),
    h4("Token -->"),
    verbatimTextOutput("SHINYPROXY_OIDC_ACCESS_TOKEN")
  )
}