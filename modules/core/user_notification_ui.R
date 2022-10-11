user_notification_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "user_notification",
    h2("My Notifications"),
    uiOutput(ns("datacall_news")),
  )
}