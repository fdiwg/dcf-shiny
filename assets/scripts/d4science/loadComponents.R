#loadComponents
loadComponents <- function(profile, sdi = TRUE){
  
  components <- list()
  
  #STORAGEHUB
  components$STORAGEHUB <- try(d4storagehub4R::StoragehubManager$new(token = profile$access$access_token, token_type = "jwt"))
  attr(components$STORAGEHUB, "description") <- "Workspace (StorageHub)"
  if(is(components$STORAGEHUB, "try-error")){
    components$STORAGEHUB <- "Error while connecting to the STORAGEHUB component. Please contact a system administrator"
    class(components$STORAGEHUB) <- "try-error"
    return(components$STORAGEHUB)
  }
  
  #WPS
  components$WPS_CONFIG <- list()
  components$WPS <- ""
  if(sdi){
    icproxy_req <- try(httr::GET(sprintf("https://registry.d4science.org/icproxy/gcube/service/ServiceEndpoint/DataAnalysis/DataMiner"),
                                 httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token))))
    if(httr::status_code(icproxy_req) == 200){
      icproxy <- XML::xmlParse(httr::content(icproxy_req, "text"))
      wps_uri = XML::xpathSApply(icproxy, "//AccessPoint/Interface/Endpoint", XML::xmlValue)[1]
      components$WPS_CONFIG$url = wps_uri
      components$WPS <- try(ows4R::WPSClient$new(
        url = wps_uri, serviceVersion = "1.0.0",
        headers = c("Authorization" = paste("Bearer", profile$access$access_token)),
        logger = "INFO"
      ), silent = TRUE)
    }else{
      class(components$WPS) <- "try-error"
    }
  }
  attr(components$WPS, "description") <- "OGC Web Processing Service - WPS (DataMiner)"
  
  #SDI
  components$GEOSERVER_CONFIG <- list()
  components$GEOSERVER <- ""
  components$GEONETWORK_CONFIG <- list()
  components$GEONETWORK <- ""
  if(sdi){
    sdi_req <- try(httr::GET(sprintf("https://sdi.d4science.org/sdi-service/gcube/service/SDI"),
                             httr::add_headers(
                               "Authorization" = paste("Bearer", profile$access$access_token)
                             )))
    if(httr::status_code(icproxy_req) == 200){
      sdi_resp <- httr::content(sdi_req)
    
      if(length(sdi_resp$geoserverClusterConfiguration)>0){
        gs_url <- gsub("http://", "https://", sdi_resp$geoserverClusterConfiguration[[1]]$baseEndpoint)
        gs_creds <- sdi_resp$geoserverClusterConfiguration[[1]]$accessibleCredentials[[1]]
        components$GEOSERVER_CONFIG <- list(url = gs_url, user = gs_creds$username, pwd = gs_creds$password)
        components$GEOSERVER <- try(geosapi::GSManager$new(
          url = gs_url,
          user = gs_creds$username, pwd = gs_creds$password,
          logger = "INFO"
        ))
      }else{
        class(components$GEOSERVER) <- "unavailable"
      }
    
      if(length(sdi_resp$geonetworkConfiguration)>0){
        gn_url <- gsub("http://", "https://", sdi_resp$geonetworkConfiguration[[1]]$baseEndpoint)
        gn_version <- paste(sdi_resp$geonetworkConfiguration[[1]]$version, collapse=".")
        gn_creds = sdi_resp$geonetworkConfiguration[[1]]$accessibleCredentials[[1]]
        components$GEONETWORK_CONFIG <- list(url = gn_url, version = gn_version, user = gn_creds$username, pwd = gn_creds$password)
        if(sdi) components$GEONETWORK <- try(geonapi::GNManager$new(
          url = gn_url,
          version = gn_version,
          user = gn_creds$username, pwd = gn_creds$password,
          logger = "INFO"
        ))
      }else{
        class(components$GEONETWORK) <- "unavailable"
      }
    
    }else{
      class(components$GEOSERVER) <- "try-error"
      class(components$GEONETWORK) <- "try-error"
    }
  }
  attr(components$GEOSERVER, "description") <- "GeoServer"
  attr(components$GEONETWORK, "description") <- "GeoNetwork"
  return(components)
}