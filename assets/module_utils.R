#listModuleProfiles
listModuleProfiles <- function(config = NULL){
  default_module_profiles <- list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
  default_module_profiles <- default_module_profiles[regexpr("i18n", default_module_profiles)<0]
  return(default_module_profiles)
}

#loadModuleProfile
loadModuleProfile <- function(filename){
  profile <- jsonlite::read_json(filename)
  if(is.null(profile$enabled)) profile$enabled <- TRUE
  if(is.null(profile$menu)) profile$menu <- FALSE
  if(is.null(profile$icon)) profile$icon <- "angle-double-right"
  return(profile)
}

#loadModuleScripts
loadModuleScripts <- function(config = NULL){
  default_module_profiles <- listModuleProfiles(config)
  modules <- data.frame(
    filename = default_module_profiles,
    name = sapply(default_module_profiles, function(x){unlist(strsplit(unlist(strsplit(x, paste0(dirname(x),"/")))[2], ".json"))[1]}),
    dirname = sapply(default_module_profiles, dirname),
    stringsAsFactors = FALSE
  )
  for(i in 1:nrow(modules)){
    module <- modules[i,]
    module_profile <- loadModuleProfile(module$filename)
    module_config = config$modules[[module$name]]
    has_config = !is.null(module_config)
    if(has_config) if(!is.null(module_config$enabled)) module_profile$enabled = module_config$enabled
    if(module_profile$enabled && !module_profile$menu){
      INFO("Loading shiny module '%s' scripts...", module$name)
      source(file.path(module$dirname, paste0(module$name, "_server.R")))
      source(file.path(module$dirname, paste0(module$name, "_ui.R")))
    }else{
      WARN("Skipping load of shiny module '%s' scripts...", module$name)
    }
  }
}

#loadModuleServers
loadModuleServers <- function(parent.session, config, profile, components){
  default_module_profiles <- listModuleProfiles(config)
  for(module_profile in default_module_profiles){
    module <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- loadModuleProfile(module_profile)
    if(outp$type != "internal" && !outp$menu){
      enabled = outp$enabled
      if(!is.null(outp$restricted)) if(outp$restricted) enabled <- any(sapply(profile$shiny_resource_access$roles, function(x){x %in% outp$roles}))
      module_config = config$modules[[module]]
      has_config = !is.null(module_config)
      if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
      if(enabled){
        INFO("Loading shiny module '%s' server functions...", module)
        server_fun_name <- paste0(module, "_server")
        server_fun <- try(eval(expr = parse(text = server_fun_name)))
        if(!is(server_fun, "try-error")){
          called <- try(server_fun(module, parent.session = parent.session, config = config, profile = profile, components = components))
          if(is(called, "try-error")){
            ERROR("Error while calling shiny module '%s'", module)
          }
        }else{
          ERROR("Error while evaluating server function '%s'", server_fun_name)
        }
      }
    }
  }
}

#loadModuleUIs
loadModuleUIs <- function(config = NULL, profile){
  default_module_profiles <- listModuleProfiles(config)
  module_uis <- lapply(default_module_profiles, function(module_profile){
    out <- NULL
    module <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- loadModuleProfile(module_profile)
    if(outp$type != "internal" && !outp$menu){
      enabled = TRUE
      if(!is.null(outp$restricted)) if(outp$restricted) enabled <- any(sapply(profile$shiny_resource_access$roles, function(x){x %in% outp$roles}))
      module_config = config$modules[[module]]
      has_config = !is.null(module_config)
      if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
      if(enabled){
        INFO("Loading shiny module '%s' UI functions...", module)
        ui_fun_name <- paste0(module, "_ui")
        ui_fun <- try(eval(expr = parse(text = ui_fun_name)))
        if(!is(ui_fun, "try-error")){
          out <- ui_fun(module)
        }else{
          ERROR("Error while evaluating UI function '%s'", ui_fun_name)
        }
      }else{
        WARN("Shiny module UI '%s' disabled!", module)
      }
    }
    return(out)
  })
  module_uis <- module_uis[!sapply(module_uis, is.null)]
  ui <- do.call("tabItems", module_uis)
  return(ui)
}

#createSidebarfromModules
sidebarMenuFromModules <- function(config = NULL, profile){
  
  #default modules
  default_module_profiles <- listModuleProfiles(config)
  
  #extend default module profiles in case custom configs are available
  module_profiles = lapply(default_module_profiles, function(module_profile){
    #read module profile
    module_profile_name <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- loadModuleProfile(module_profile)
    outp$source <- module_profile
    outp$module <- module_profile_name
    if(!is.null(outp$restricted)) if(outp$restricted) outp$enabled <- any(sapply(profile$shiny_resource_access$roles, function(x){x %in% outp$roles}))
    #overwrite with config module definition
    m_config <- config$modules[[outp$module]]
    if(!is.null(m_config)){
      for(m_config_propname in names(m_config)){
        if(m_config_propname %in% names(outp)){
          outp[[m_config_propname]] <- m_config[[m_config_propname]]
        }
      }
    }
    return(outp)
  })
  module_profiles <- module_profiles[sapply(module_profiles, function(x){x$enabled})]
  #remove internal modules
  module_profiles <- module_profiles[sapply(module_profiles, function(x){x$type != "internal"})]
  
  #TODO custom modules?
  
  #default structure
  default_structure_menu_items <- module_profiles[sapply(module_profiles, function(x){x$type == "item"})]
  default_structure_menu_items <- default_structure_menu_items[order(sapply(default_structure_menu_items, function(x){x$rank}))]
  structure_menu_items <- default_structure_menu_items
  names(structure_menu_items) <- sapply(structure_menu_items, function(x){x$name})
  print(structure_menu_items)
  #overwrite with custom structure
  if(!is.null(config$structure)) structure_menu_items <- config$structure
  
  #sidebar UI
  do.call("sidebarMenu", c(id="dcf-tabs", lapply(names(structure_menu_items), function(menu_item_name){
    
    INFO("Loading shiny menu item '%s'...", menu_item_name)
    #menu item
    menu_item <- structure_menu_items[[menu_item_name]]
    menu_item_enabled <- menu_item$enabled

    #profiles of subitem modules associated to this menu item
    menu_subitem_profiles <- module_profiles[sapply(module_profiles, function(x){x$type == "subitem"})]
    if(length(menu_subitem_profiles)>0) menu_subitem_profiles <- menu_subitem_profiles[sapply(menu_subitem_profiles, function(x){
      filtered <- !is.null(x$parent)
      if(filtered) filtered <- x$parent == menu_item_name
      filtered
    })]
    if(length(menu_subitem_profiles)>0) menu_subitem_profiles <- menu_subitem_profiles[order(sapply(menu_subitem_profiles, function(x){ifelse(!is.null(x$rank), x$rank, 1)}))]
    #ui for menu item
    if(length(menu_subitem_profiles)>0){
      print(menu_subitem_profiles)
      INFO("Loading shiny menu sub-items for '%s'...", menu_item_name)
      do.call("menuItem", c(
        text = menu_item$title, tabName = menu_item_name,
        lapply(menu_subitem_profiles, function(item_profile){
          INFO("Loading shiny menu sub-item '%s'...", item_profile$module)
          if(!item_profile$enabled) return(NULL)
          icon = shiny::icon(item_profile$icon)
          menuSubItem(item_profile$title, tabName = item_profile$module, icon = icon)
        })
      ))
    }else{
      menuItem(text = menu_item$title, tabName = menu_item_name)
    }
  })))
}
