dashboard_server <- function(input, output, session, profile, parent.session){
  
  ns <- session$ns
  
  output$welcome <- renderUI({
    h2(sprintf("Welcome %s!", profile$name))
  })
  
  output$status <- renderUI({
    if(PROFILE$expired){
      tags$span(shiny::icon(c('times-circle')), "Token is expired", style="color:red;")
    }else{
      tags$span(shiny::icon(c('check-circle')), "Token is valid", style="color:green;")
    }
  })
  
  output$roles <- renderDataTable({
    data.frame(
      resource = sapply(names(profile$resource_access), URLdecode),
      roles = sapply(profile$resource_access, function(x){ paste0(x$roles, collapse = ", ")})
    )
  })
  
}