data_submissions_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_submissions",
    DT::dataTableOutput(ns("tbl_data_submissions"))
  )
}