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
    if(is.null(cfg$dcf$tasks)) stop("No dcf 'tasks' defined in configuration")
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
  return(config$dcf$roles)
}

#getReportingEntityCodes
getReportingEntityCodes <- function(config){
  return(config$dcf$reporting_entities$codelist_ref)
}