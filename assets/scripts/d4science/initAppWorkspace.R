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