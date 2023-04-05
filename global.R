#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize=200*1024^2) #increase upload size to 200Mb

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

#Add resource path towards temporary directory for iframe display
#---------------------------------------------------------------------------------------
addResourcePath(prefix = "tmp", directoryPath = tempdir())
print(tempdir())

#modules
#---------------------------------------------------------------------------------------
loadModuleScripts()

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

