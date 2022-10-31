function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "wecafc-firms_publisher",
    h3("SDI Data publisher"),hr(),
    fluidRow(
      div(
        class = "col-md-2",
        uiOutput(ns("task_wrapper"))
      )
    )
  )
  
}