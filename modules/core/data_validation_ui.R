data_validation_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_validation",
    tabsetPanel(id = "tabs",
      tabPanel("Data",
      h2("Test validity of your data file"),
      p("A descriptive text to guide the user"),
      fluidRow(
        column(3,
          uiOutput(ns("task_wrapper")),
          uiOutput(ns("format_wrapper")),
          uiOutput(ns("file_wrapper")),
          uiOutput(ns("run_wrapper"))
        ),
        column(9,
          DTOutput(ns("dataView"))
        )
      )
    )
    )
  )
}