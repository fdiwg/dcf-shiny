data_entry_editor_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboard::tabItem(
    tabName = "data_entry_editor",
    h3("Edit data"),hr(),
    fluidRow(
      div(
        class = "col-md-2",
        uiOutput(ns("task_wrapper")),
        uiOutput(ns("reporting_entity_wrapper")),
        uiOutput(ns("format_wrapper"))
      ),
      uiOutput(ns("download_wrapper")),
      
    ),
    uiOutput(ns("file_origin_wrapper")),
    uiOutput(ns("file_wrapper")),
    uiOutput(ns("save_wrapper")),
    uiOutput(ns("table_wrapper"))
  )
}