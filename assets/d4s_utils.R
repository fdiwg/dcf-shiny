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
  
  #SDI
  components$GEOSERVER <- ""
  components$GEONETWORK <- ""
  sdi_req <- try(httr::GET(sprintf("http://sdi.d4science.org/sdi-service/gcube/service/SDI"),
                           httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token))))
  if(httr::status_code(icproxy_req) == 200){
    sdi_resp <- content(sdi_req)
    
    if(!is.null(sdi_resp$geoserverClusterConfiguration)){
      gs_creds <- sdi_resp$geoserverClusterConfiguration[[1]]$accessibleCredentials[[1]]
      components$GEOSERVER <- try(geosapi::GSManager$new(
        url = gsub("http://", "https://", sdi_resp$geoserverClusterConfiguration[[1]]$baseEndpoint),
        user = gs_creds$username, pwd = gs_creds$password,
        logger = "DEBUG"
      ))
    }else{
      class(components$GEOSERVER) <- "unavailable"
    }
    
    if(!is.null(sdi_resp$geonetworkConfiguration)){
      gn_creds = sdi_resp$geonetworkConfiguration[[1]]$accessibleCredentials[[1]]
      components$GEONETWORK <- try(geonapi::GNManager$new(
        url = gsub("http://", "https://", sdi_resp$geonetworkConfiguration[[1]]$baseEndpoint),
        version = paste(sdi_resp$geonetworkConfiguration[[1]]$version, collapse="."),
        user = gn_creds$username, pwd = gn_creds$password,
        logger = "DEBUG"
      ))
    }else{
      class(components$GEONETWORK) <- "unavailable"
    }
    
  }else{
    class(components$GEOSERVER) <- "try-error"
    class(components$GEONETWORK) <- "try-error"
  }
  attr(components$GEOSERVER, "description") <- "GeoServer"
  attr(components$GEONETWORK, "description") <- "GeoNetwork"
  return(components)
}

#getAppUserRoles
getAppUserRoles <- function(profile){
  req <- httr::with_verbose(
    httr::GET("https://cdn.d4science.org/services/d4s-vre-manager/users-with-roles?apps=1c2231ee-1779-4e50-9769-3ac9feb57c88", 
        httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token))
    )
  )
  json <- httr::content(req)
  names(json) <- sapply(json, function(x){x$username})
  return(json)
}

#initAppWorkspace
initAppWorkspace <- function(config, profile, components){
  if(is.null(profile$access)) return(NULL)
  pool <- components$POOL
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
    }
    dcf_managers <- getDBUsersWithRole(pool = pool, profile = profile, role = config$dcf$roles$manager)
    shared <- SH$shareItem(itemPath = config$dcf$user_workspace, defaultAccessType = "WRITE_ALL", users = dcf_managers$username) #TODO check it works over existing sharing
  }
  return(ws)
}