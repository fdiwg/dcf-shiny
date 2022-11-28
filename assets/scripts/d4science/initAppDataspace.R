#initAppDataspace
initAppDataspace <- function(config, profile, components){
  if(is.null(profile$access)) return(NULL)
  pool <- components$POOL
  SH <- components$STORAGEHUB
  
  ws <- NULL
  if(is(SH, "StoragehubManager")){
    ws <- SH$getWSItemID( parentFolderID = SH$getWSVREFolderID(), itemPath = config$dcf$workspace )
    if(is.null(ws)){
      ws <- SH$createFolder(folderID = SH$getWSVREFolderID(),name = config$dcf$workspace, description = sprintf("Dataspace for the '%s' application", config$dcf$workspace))
      if(is.null(ws)){
        ERROR("Failed to create app dataspace '%s'", config$dcf$workspace)
        stop(sprintf("Failed to create app dataspace '%s'", config$dcf$workspace))
      }
    }else{
      INFO("VRE dcf-shiny data space ID: %s", ws)
    }
  }
  return(ws)
}