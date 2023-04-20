dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "dashboard",
    uiOutput(ns("welcome")),hr(),
    uiOutput(ns("user_roles")),
    uiOutput(ns("ressource_management")),
    uiOutput(ns("datacall_stat")),
    uiOutput(ns("token_wrapper"))
  )
}