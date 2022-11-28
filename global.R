#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#utils
#---------------------------------------------------------------------------------------
scripts <- as.list(list.files("assets/scripts", recursive = T, full.names = TRUE))
invisible(lapply(scripts, source))

#packages
#---------------------------------------------------------------------------------------
loadAppPackages()

#env
#---------------------------------------------------------------------------------------
try(dotenv::load_dot_env(file = ".REnviron"), silent = TRUE)

#global variables / environment
#---------------------------------------------------------------------------------------
jwt <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")

#D4S components
#---------------------------------------------------------------------------------------
PROFILE <- loadProfile(jwt)
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
PROFILE <- fetchProfileRoles(pool = COMPONENTS$POOL, profile = PROFILE)

#Add resource path towards temporary directory for iframe display
#---------------------------------------------------------------------------------------
addResourcePath(prefix = "tmp", directoryPath = tempdir())
print(tempdir())

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
