dashboard_server <- function(id, parent.session, config, profile, pool){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
  
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
        if("system admin" %in% profile$context_resource_access$roles){
          tags$span(profile$jwt)
        }else{
          tags$span("<hidden>")
        }
      })
      
      output$roles <- renderDataTable({
        data.frame(
          resource = URLdecode(profile$context),
          roles = paste0(profile$context_resource_access$roles, collapse=", ")
        )
      })
      #----------------------------------------------------------------------------------- 
    }
  )
  
}