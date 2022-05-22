monitor_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #D4science resources
      resources <- names(components)
      
      output$resources <- renderDataTable(
        do.call("rbind", lapply(resources, function(resource_name){
          resource <- components[[resource_name]]
          data.frame(
            name = resource_name, 
            description = attr(resource, "description"), 
            status = {
              if(!is(resource, "try-error")){
                paste0(tags$span(shiny::icon("check-circle"), title = "Connected", style = "color:green;", "Connected"), collapse="")
              }else{
                paste0(tags$span(shiny::icon("times-circle"), title = "Failed to connect", style = "color:red;", "Failed to connect"), collapse="")
              }
            }
          )
        })),
        server = FALSE,
        rownames = FALSE,
        escape= FALSE
      )
      #-----------------------------------------------------------------------------------
    }
  )
  
}