dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "dashboard",
    uiOutput(ns("welcome")),hr(),
    uiOutput(ns("status")),br(),
    uiOutput(ns("token_wrapper")),br(),
    dataTableOutput(ns("roles"))
  )
}