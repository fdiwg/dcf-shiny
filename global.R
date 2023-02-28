#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#packages
#---------------------------------------------------------------------------------------
loadAppPackages()

#utils
#---------------------------------------------------------------------------------------
scripts <- as.list(list.files("assets/scripts", recursive = T, full.names = TRUE))
invisible(lapply(scripts, source))

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

