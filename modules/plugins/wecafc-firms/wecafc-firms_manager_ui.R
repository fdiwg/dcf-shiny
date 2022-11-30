function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    h3("RDB datasets"),hr(),
    DT::DTOutput(ns("tbl_rdb_datasets"))
  )
  
}