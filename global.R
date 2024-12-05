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
#loadAppPackages() #not detected by renv
library(jsonlite)
library(R6)
library(dotenv)
library(stringi)
library(tibble)
library(dplyr)
library(yaml)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(httr)
library(mime)
library(XML)
library(jose)
library(benchmarkme)
library(zip)
library(DBI)
library(pool)
library(RPostgres)
library(uuid)
library(DT)
library(whisker)
library(readxl)
library(readr)
library(tidyr)
library(lubridate)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(geosapi)
library(geonapi)
library(ows4R)
library(jsonld)
library(rdflib)
library(waiter)
library(plotly)
library(openxlsx)
library(rhandsontable)
library(rclipboard)
library(shinyWidgets)
library(geometa)
library(d4storagehub4R)
library(atom4R)
library(geonode4R)
library(geoflow)
library(vrule)

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

