monitor_server <- function(input, output, session, profile, parent.session){
  
  ns <- session$ns
  
  resources <- c(
    "Workspace (StorageHub)", 
    "DataMiner (WPS)"
  )
  
  output$resources <- renderDataTable(
    data.frame(
      resource = resources,
      status = sapply(resources, function(resource){
        switch(resource,
           "Workspace (StorageHub)" = {
             STORAGEHUB <- try(d4storagehub4R::StoragehubManager$new(token = PROFILE$access$access_token, token_type = "jwt"))
             if(!is(STORAGEHUB, "try-error")){
               paste0(tags$span(shiny::icon("check-circle"), title = "Connected", style = "color:green;"), collapse="")
             }else{
               paste0(tags$span(shiny::icon("times-circle"), title = "Failed to connect", style = "color:red;"), collapse="")
             }
            },
           "DataMiner (WPS)" = {
             paste0(tags$span(shiny::icon("exclamation-circle"), title = "Not evaluated", style = "color:gray;"), collapse="")
           }
        )
      })
    ),
    server = FALSE,
    rownames = FALSE,
    escape= FALSE
  )
  
}