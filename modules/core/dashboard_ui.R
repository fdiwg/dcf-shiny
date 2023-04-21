dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "dashboard",
    uiOutput(ns("welcome")),hr(),
    uiOutput(ns("user_roles")),
    shinycssloaders::withSpinner(uiOutput(ns("datacall_stat")),type=5),
    uiOutput(ns("ressource_management"))
  )
}