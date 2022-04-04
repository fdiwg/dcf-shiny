# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  fetchProfile <- function(jwt){
    (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])
    out_jwt <- jsonlite::parse_json(rawToChar(jose::base64url_decode(strings[2])))
    out_jwt$expired <- as(Sys.time(), "numeric") > out_jwt$exp
    
    if(!out_jwt$expired){
      req <- httr::with_verbose(httr::POST(
         "https://accounts.d4science.org/auth/realms/d4science/protocol/openid-connect/token",
         encode = "form",
         add_headers("Authorization" = paste("Bearer", jwt)),
         body = list(
          grant_type = I("urn:ietf:params:oauth:grant-type:uma-ticket"),
          audience = "%2Fd4science.research-infrastructures.eu%2FFARM%2FWECAFC-FIRMS" #TODO needs config
         )
       ))
      if(httr::status_code(req)==200){
        out_jwt$access <- content(req)
      }
    }
    
    
    return(out_jwt)
  }
  
  jwt <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")
  PROFILE <- fetchProfile(jwt)
  
  shiny::callModule(dashboard_server, "dashboard", profile = PROFILE, parent.session = session)
  shiny::callModule(monitor_server, "monitor", profile = PROFILE, parent.session = session)
}