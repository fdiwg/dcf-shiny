#loadDBI
loadDBI <- function(config){
  #DB
  POOL <- try(pool::dbPool(
    drv = DBI::dbDriver(config$dbi$drv),
    dbname = config$dbi$dbname,
    host = config$dbi$host,
    port = config$dbi$port,
    user = config$dbi$user,
    password = config$dbi$password
  ))
  attr(POOL, "description") <- "Database"
  return(POOL)
}

#loadComponents
loadComponents <- function(profile){
  
  components <- list()
  
  #STORAGEHUB
  components$STORAGEHUB <- try(d4storagehub4R::StoragehubManager$new(token = profile$access$access_token, token_type = "jwt"))
  attr(components$STORAGEHUB, "description") <- "Workspace (StorageHub)"
  
  #WPS
  components$WPS <- ""
  icproxy_req <- try(httr::GET(sprintf("https://registry.d4science.org/icproxy/gcube/service/ServiceEndpoint/DataAnalysis/DataMiner"),
                           httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token))))
  if(httr::status_code(icproxy_req) == 200){
    icproxy <- XML::xmlParse(httr::content(icproxy_req, "text"))
    wps_uri = XML::xpathSApply(icproxy, "//AccessPoint/Interface/Endpoint", XML::xmlValue)[1]
    if(!is.null(profile$access)){
      components$WPS <- try(ows4R::WPSClient$new(
        url = wps_uri, serviceVersion = "1.0.0",
        headers = c("Authorization" = paste("Bearer", profile$access$access_token)),
        logger = "DEBUG"
      ))
    }
  }else{
    class(components$WPS) <- "try-error"
  }
  attr(components$WPS, "description") <- "OGC Web Processing Service - WPS (DataMiner)"
  
  return(components)
}

#initAppWorkspace
initAppWorkspace <- function(config, profile, components){
  if(is.null(profile$access)) return(NULL)
  SH <- components$STORAGEHUB
  
  ws <- NULL
  if(is(SH, "StoragehubManager")){
    ws <- SH$getWSItemID(itemPath = config$dcf$user_workspace)
    if(is.null(ws)){
      ws <- SH$createFolder(name = config$dcf$user_workspace, description = sprintf("Your personal workspace for the '%s' application", config$dcf$user_workspace))
      if(is.null(ws)){
        ERROR("Failed to create app workspace '%s'", config$dcf$user_workspace)
        stop(sprintf("Failed to create app workspace '%s'", config$dcf$user_workspace))
      }
      shared <- SH$shareItem(itemPath = config$dcf$user_workspace, defaultAccessType = "WRITE_ALL", users = "emmanuel.blondel")
    }
  }
  return(ws)
}