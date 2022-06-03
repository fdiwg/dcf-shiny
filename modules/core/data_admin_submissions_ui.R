data_admin_submissions_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_admin_submissions",
    DT::dataTableOutput(ns("tbl_all_submissions"))
  )
}