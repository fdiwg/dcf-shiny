function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = id,
    h3("Data availability"),
    fluidRow(
      div(
        class = "col-md-2",
        uiOutput(ns("task_selector"))
      ),
      div(
        class = "col-md-2",
        uiOutput(ns("entities_selector"))
      )
    ),
    fluidRow(
      withSpinner(plotlyOutput(ns("heatmap")),type=4)
    )
  )
  
}