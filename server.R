# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  decodeJWT <- function(jwt){
    (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])
    out_jwt <- jsonlite::parse_json(rawToChar(jose::base64url_decode(strings[2])))
    out_jwt$expired <- as(Sys.time(), "numeric") > PROFILE$exp
    return(out_jwt)
  }
  
  jwt <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")
  PROFILE <- decodeJWT(jwt)
  
  shiny::callModule(dashboard_server, "dashboard", profile = PROFILE, parent.session = session)
   
}