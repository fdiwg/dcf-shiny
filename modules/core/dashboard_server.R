dashboard_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
  
      pool <- components$POOL
      
      ns <- session$ns
      
      output$welcome <- renderUI({
        h2(sprintf("Welcome %s!", profile$name))
      })
      
      output$status <- renderUI({
        if(profile$expired){
          tags$span(shiny::icon(c('times-circle')), "Token is expired", style="color:red;")
        }else{
          tags$span(shiny::icon(c('check-circle')), "Token is valid", style="color:green;")
        }
      })
      
      output$token <- renderUI({
        if("system admin" %in% profile$shiny_resource_access$roles){
          tags$span(profile$jwt)
        }else{
          tags$span("<hidden>")
        }
      })
      
      output$roles <- renderDataTable({
        data.frame(
          resource = URLdecode(config$dcf$shiny_resource_access),
          roles = paste0(profile$shiny_resource_access$roles, collapse=", ")
        )
      })
      #----------------------------------------------------------------------------------- 
    }
  )
  
}