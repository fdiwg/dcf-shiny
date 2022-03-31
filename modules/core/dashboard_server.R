dashboard_server <- function(input, output, session, profile, parent.session){
  
  ns <- session$ns
  
  output$welcome <- renderUI({
    h2(sprintf("Welcome %s!", profile$name))
  })
  
  output$roles <- renderDataTable({
    data.frame(
      resource = sapply(names(profile$resource_access), URLdecode),
      roles = sapply(profile$resource_access, function(x){ paste0(x$roles, collapse = ", ")})
    )
  })
  
}