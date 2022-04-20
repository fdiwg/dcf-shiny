#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#utils
#---------------------------------------------------------------------------------------
source("assets/utils.R")
source("assets/package_utils.R")
source("assets/module_utils.R")
source("assets/ui_utils.R")

#packages
#---------------------------------------------------------------------------------------
loadAppPackages()

#config
#---------------------------------------------------------------------------------------
#default config_file path for DEPLOYMENT
config_file <- file.path(getwd(), "configs/config.yml")

#local configuration
#If you are an R developer, you need to create a .REnviron file (no file extension) in /dcf-shiny dir
#The file should include the local path for your shiny config file in that way:
#DCF_SHINY_CONFIG=<your config path>
local_config_file <- Sys.getenv("DCF_SHINY_CONFIG")
if(nzchar(local_config_file)) config_file <- local_config_file
CONFIG <- suppressWarnings(yaml::read_yaml(config_file))

#language (in case not part of configuration)
if(is.null(CONFIG$language)) CONFIG$language <- "en"

#DB connections
#---------------------------------------------------------------------------------------
POOL <- try(pool::dbPool(
  drv = DBI::dbDriver(CONFIG$dbi$drv),
  dbname = CONFIG$dbi$dbname,
  host = CONFIG$dbi$host,
  port = CONFIG$dbi$port,
  user = CONFIG$dbi$user,
  password = CONFIG$dbi$password
))

#global variables / environment
#---------------------------------------------------------------------------------------
fetchProfile <- function(jwt){
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])
  out_jwt <- jsonlite::parse_json(rawToChar(jose::base64url_decode(strings[2])))
  out_jwt$expired <- as(Sys.time(), "numeric") > out_jwt$exp
  out_jwt$context <- names(out_jwt$resource_access)[1]
  if(!out_jwt$expired){
    req <- httr::with_verbose(httr::POST(
      "https://accounts.d4science.org/auth/realms/d4science/protocol/openid-connect/token",
      encode = "form",
      add_headers("Authorization" = paste("Bearer", jwt)),
      body = list(
        grant_type = I("urn:ietf:params:oauth:grant-type:uma-ticket"),
        audience = URLencode(out_jwt$context, reserved = T)
      )
    ))
    if(httr::status_code(req)==200){
      out_jwt$access <- content(req)
    }
  }
  out_jwt$context_resource_access <- out_jwt$resource_access[[1]]
  out_jwt$jwt <- jwt
  return(out_jwt)
}

jwt <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")
PROFILE <- fetchProfile(jwt)

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
  DBI::dbDisconnect(POOL)
})
