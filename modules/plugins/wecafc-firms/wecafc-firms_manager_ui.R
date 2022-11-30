function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    h3("RDB datasets"),hr(),
    fluidRow(
      DT::DTOutput(ns("tbl_rdb_datasets"))
    )
  )
  
}