#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#utils
#---------------------------------------------------------------------------------------
source("assets/utils.R")
source("assets/package_utils.R")
source("assets/module_utils.R")
source("assets/d4s_utils.R")
source("assets/ui_utils.R")
source("assets/message_utils.R")
source("assets/user_utils.R")
source("assets/dcf_utils.R")

#packages
#---------------------------------------------------------------------------------------
loadAppPackages()

#global variables / environment
#---------------------------------------------------------------------------------------
#fetchProfile
fetchProfile <- function(jwt){
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])
  out_jwt <- jsonlite::parse_json(rawToChar(jose::base64url_decode(strings[2])))
  out_jwt$expired <- as(Sys.time(), "numeric") > out_jwt$exp
  out_jwt$jwt <- jwt
  
  #TODO how to know the current VRE context?
  vre_contexts <- names(out_jwt$resource_access)[startsWith(names(out_jwt$resource_access), "%2Fd4science.research-infrastructures.eu")]
  if(length(vre_contexts)==0) stop("No VRE context available!")
  out_jwt$vre_context <- vre_contexts[[1]]
  
  out_jwt$vre_resource_access <- out_jwt$resource_access[[out_jwt$vre_context]]
  out_jwt$shiny_resource_access <- out_jwt$resource_access[["dcf-shiny"]]
  if(out_jwt$expired) stop("JWT token is expired")
  if(!out_jwt$expired){
    req <- httr::with_verbose(httr::POST(
      "https://accounts.d4science.org/auth/realms/d4science/protocol/openid-connect/token",
      encode = "form",
      add_headers("Authorization" = paste("Bearer", jwt)),
      body = list(
        grant_type = I("urn:ietf:params:oauth:grant-type:uma-ticket"),
        audience = URLencode(out_jwt$vre_context, reserved = TRUE)
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

#D4S components
#---------------------------------------------------------------------------------------
COMPONENTS <- loadComponents(profile = PROFILE)


#TODO current config from file, next to get from Workspace URL inherited from ICPROXY
#---------------------------------------------------------------------------------------
#default config_file path for DEPLOYMENT (hidden file)
config_file <- COMPONENTS$STORAGEHUB$downloadItemByPath("dcf-shiny-config/config.yml", wd = tempdir())

#local configuration
#If you are an R developer, you need to create a .REnviron file (no file extension) in /dcf-shiny dir
#The file should include the local path for your shiny config file in that way:
#DCF_SHINY_CONFIG=<your config path>
local_config_file <- Sys.getenv("DCF_SHINY_CONFIG")
if(nzchar(local_config_file)) config_file <- local_config_file
CONFIG <- read_dcf_config(file = config_file)

#DBI component to add
#---------------------------------------------------------------------------------------
COMPONENTS$POOL <- loadDBI(config = CONFIG)

#scripts
#---------------------------------------------------------------------------------------

#local datasets
#---------------------------------------------------------------------------------------

#modules
#---------------------------------------------------------------------------------------
loadModuleScripts()

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

#onStop
#---------------------------------------------------------------------------------------
onStop(function(){
  DBI::dbDisconnect(COMPONENTS$POOL)
})
