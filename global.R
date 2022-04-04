#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#config
#---------------------------------------------------------------------------------------

#packages
#---------------------------------------------------------------------------------------
packages <- jsonlite::read_json('./package.json')
list_of_packages <- sapply(packages, function(x){
  pkg <- x$package
  parts <- unlist(strsplit(x$package,"/"))
  if(length(parts)>1) pkg <- parts[2]
  return(pkg)
})
invisible(lapply(list_of_packages, function(x) {
  require(x,character.only = TRUE, quietly = TRUE)
}))

#global variables / environment
#---------------------------------------------------------------------------------------

#scripts
#---------------------------------------------------------------------------------------

#local datasets
#---------------------------------------------------------------------------------------

#modules
#---------------------------------------------------------------------------------------
source("modules/core/dashboard_server.R")
source("modules/core/dashboard_ui.R")
source("modules/core/monitor_server.R")
source("modules/core/monitor_ui.R")

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

#onStop
#---------------------------------------------------------------------------------------
onStop(function(){
  
})
