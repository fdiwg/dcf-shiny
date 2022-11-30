# Run the application 
shinyApp(
  server = server, 
  ui = ui,
  onStop(function(){
    if(!is.null(COMPONENTS$POOL)) DBI::dbDisconnect(COMPONENTS$POOL)
  })
)