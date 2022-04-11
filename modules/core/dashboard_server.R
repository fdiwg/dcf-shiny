dashboard_server <- function(input, output, session, profile, parent.session){
  
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
  
  output$roles <- renderDataTable({
    data.frame(
      resource = URLdecode(profile$context),
      roles = paste0(profile$context_resource_access$roles, collapse=", ")
    )
  })
  
}