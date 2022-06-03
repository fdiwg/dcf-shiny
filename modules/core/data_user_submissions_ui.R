data_user_submissions_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_user_submissions",
    h3("My submissions"),hr(),
    withSpinner(DT::dataTableOutput(ns("tbl_my_submissions")))
  )
}