data_submissions_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_submissions",
    actionButton(ns("refresh"),"Refresh",icon = icon("sync-alt")),
    DT::dataTableOutput(ns("tbl_data_submissions"))
  )
}
