dashboard_server <- function(input, output, session, parent.session){
  
  ns <- session$ns
  
  output$SHINYPROXY_USERNAME <- renderPrint( Sys.getenv("SHINYPROXY_USERNAME"))
  output$SHINYPROXY_OIDC_ACCESS_TOKEN <- renderPrint( Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN") )
  
}