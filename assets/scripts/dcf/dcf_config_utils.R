#read_dcf_config
read_dcf_config <- function(file){
  
  cfg <- suppressWarnings(yaml::read_yaml(file))
  
  #language
  if(is.null(cfg$language)) cfg$language <- "en"
  
  #dcf framework
  if(is.null(cfg$dcf)){
    stop("No 'dcf' frame in configuration")
  }else{
    if(is.null(cfg$dcf$name)) stop("No dcf 'name' defined in configuration")
    if(is.null(cfg$dcf$context)) stop("No dcf 'context' defined in configuration")
    if(is.null(cfg$dcf$workspace)) stop ("No dcf 'workspace' defined in configuration")
    if(is.null(cfg$dcf$roles)) stop("No dcf 'roles' defined in configuration")
    if(is.null(cfg$dcf$roles$submitter)) stop("No dcf roles 'submitter' name defined in configuration")
    if(is.null(cfg$dcf$roles$manager)) stop("No dcf roles 'manager' name defined in configuration")
    
    #tasks
    if(is.null(cfg$dcf$tasks)){
      if(is.null(cfg$dcf$receiver)){
        stop("No dcf 'tasks' or 'receiver' defined in configuration")
      }else{
        receiver = repfishr::reporting_receiver$new(id = cfg$dcf$receiver, name = cfg$dcf$receiver)
        cfg$dcf$tasks <- receiver$getTaskDefinitions(raw = TRUE)
      }
    }else{ 
      cfg$dcf$tasks = lapply(names(cfg$dcf$tasks), function(x){
        task = cfg$dcf$tasks[[x]]
        if(!is.null(task$ref)){
          #read from file
          task = switch(mime::guess_type(task$ref),
            "text/yaml" = yaml::read_yaml(task$ref),
            "application/json" = jsonlite::read_json(task$ref)
          )
        }
        task$id = x
        return(task)
      })
    }
  }
  
  #reporting entity
  reporting_entity <- cfg$dcf$reporting_entities
  if(!is.null(reporting_entity)){
    if(is.null(reporting_entity$name)){
      stop("No name for reporting entity")
    }
    if(is.null(reporting_entity$codelist_ref_url)){
      stop(sprintf("No codelist ref URL for user property '%s'", reporting_entity$name))
    }
    cfg$dcf$reporting_entities$codelist_ref <- readr::read_csv(cfg$dcf$reporting_entities$codelist_ref_url)
    if(is.null(cfg$dcf$reporting_entities$validation)) cfg$dcf$reporting_entities$validation <- TRUE
  }
  
  return(cfg)
}

#getAppRoles
getAppRoles <- function(config){
  roles = names(config$dcf$roles)
  names(roles) = config$dcf$roles
  return(roles)
}

#getAllRoles
getAllRoles <- function(config){
  all_roles = list(getAppRoles(config))
  for(group_name in names(config$dcf$groups)){
    group_roles <- paste(group_name, names(config$dcf$roles), sep = ":")
    names(group_roles) <- paste(group_name, config$dcf$roles)
    all_roles[[length(all_roles)+1]] <- group_roles
  }
  names(all_roles) <- c("Main roles", config$dcf$groups)
  return(all_roles)
}

#getReportingEntityCodes
getReportingEntityCodes <- function(config){
  return(config$dcf$reporting_entities$codelist_ref)
}
