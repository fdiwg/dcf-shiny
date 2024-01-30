#listModuleProfiles
listModuleProfiles <- function(config = NULL){
  default_module_profiles <- list.files(path = "./modules/core", pattern = ".json", recursive = TRUE, full.names = TRUE)
  default_module_profiles <- default_module_profiles[regexpr("i18n", default_module_profiles)<0]
  return(default_module_profiles)
}

#listPlugins
listPlugins <- function(config){
  return(config$plugins)
}

#listPluginProfiles
listPluginProfiles <- function(config){
  plugins = listPlugins(config)
  plugin_profiles = sapply(plugins, function(x){x$def})
  names(plugin_profiles) = NULL
  return(plugin_profiles)
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

#check_module_server_formals
check_module_server_formals <- function(module, server_fun){
  std_module_args <- c("id", "parent.session", "config", "profile", "components","reloader")
  if(!all(names(formals(server_fun)) == std_module_args )){
    stop(sprintf("Module '%s', server function arguments do not match module requirements. Mandatory arguments: %s", 
                 module, paste(std_module_args, collapse=","))) 
  }
}

#check_module_ui_formals
check_module_ui_formals <- function(module, ui_fun){
  std_module_args <- c("id")
  if(!all(names(formals(ui_fun)) == std_module_args )){
    stop(sprintf("Module '%s', UI function arguments do not match module requirements. Mandatory arguments: %s", 
                 module, paste(std_module_args, collapse=","))) 
  }
}

#loadModuleServers
loadModuleServers <- function(parent.session, config, profile, components,reloader){
  INFO("=> Loading Module Servers")
  default_module_profiles <- listModuleProfiles(config)
  if(!is.null(reloader())){
    reloader_profile<-loadModuleProfile(paste0("./modules/core/",reloader(),".json"))
    modules_to_reload<-reloader_profile$linked_modules
    if(length(modules_to_reload)>0){
      INFO("Module '%s' has triggered reloading event. The following modules are going to be reload: %s",reloader(),paste0(modules_to_reload,collapse =", "))
      default_module_profiles<-paste0("./modules/core/",modules_to_reload,".json")
    }else{
      return(NULL)
    }
  }
  for(module_profile in default_module_profiles){
    module <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- loadModuleProfile(module_profile)
    if(outp$type != "internal" && !outp$menu){
      enabled = outp$enabled
      if(!is.null(outp$restricted)) if(outp$restricted){
        module_ok_with_roles <- any(sapply(profile$shiny_app_roles, function(x){x %in% outp$roles}))
        INFO("Module '%s' Server %s (at least one of the profile roles [%s] do%s match module roles [%s]", 
             module, 
             ifelse(module_ok_with_roles, "enabled", "disabled"),
             paste0(unlist(profile$shiny_app_roles), collapse=","),
             ifelse(module_ok_with_roles, "", " NOT"),
             paste0(unlist(outp$roles), collapse=",")
        )
        enabled = module_ok_with_roles
      }
      module_config = config$modules[[module]]
      has_config = !is.null(module_config)
      if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
      if(enabled){
        INFO("%s shiny module '%s' server functions...",ifelse(!is.null(reloader()),"Reloading","Loading"),module)
        server_fun_name <- paste0(module, "_server")
        server_fun <- try(eval(expr = parse(text = server_fun_name)), silent = TRUE)
        if(!is.null(server_fun)){
          if(!is(server_fun, "try-error")){
            #check formals
            check_module_server_formals(module, server_fun)
            #call server function
            
            called <- try(server_fun(module, parent.session = parent.session, config = config, profile = profile, components = components, reloader = reloader), silent = TRUE)
            if(is(called, "try-error")){
              ERROR("Error while calling shiny module '%s'", module)
            }else{
              if(!is.null(called)){
                  
                }
            }
          }else{
            ERROR("Error while evaluating server function '%s'", server_fun_name)
          }
        }
      }
    }
  }
}

#loadModuleUIs
loadModuleUIs <- function(config = NULL, profile){
  INFO("=> Loading Module UIs")
  default_module_profiles <- listModuleProfiles(config)
  module_uis <- lapply(default_module_profiles, function(module_profile){
    out <- NULL
    module <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- loadModuleProfile(module_profile)
    if(outp$type != "internal" && !outp$menu){
      enabled = TRUE
      if(!is.null(outp$restricted)) if(outp$restricted){
        module_ok_with_roles <- any(sapply(profile$shiny_app_roles, function(x){x %in% outp$roles}))
        INFO("Module '%s' Server %s (at least one of the profile roles [%s] do%s match module roles [%s]", 
             module, 
             ifelse(module_ok_with_roles, "enabled", "disabled"),
             paste0(unlist(profile$shiny_app_roles), collapse=","),
             ifelse(module_ok_with_roles, "", " NOT"),
             paste0(unlist(outp$roles), collapse=",")
        )
        enabled = module_ok_with_roles
      }
      if(module == "data_admin_processing" && is.null(config$dcf$processing_module)) enabled <- FALSE
      module_config = config$modules[[module]]
      has_config = !is.null(module_config)
      if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
      if(enabled){
        INFO("Loading shiny module '%s' UI functions...", module)
        ui_fun_name <- paste0(module, "_ui")
        ui_fun <- try(eval(expr = parse(text = ui_fun_name)), silent = TRUE)
        if(!is(ui_fun, "try-error")){
          #check formals
          check_module_ui_formals(module, ui_fun)
          #call ui function
          out <- ui_fun(module)
          if(is(out, "try-error")){
            ERROR("Error while calling shiny module '%s' UI function", module)
          }
          INFO("UI function loaded for module '%s'", module)
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
  return(module_uis)
}

#loadPluginServers
loadPluginServers <- function(parent.session, config, profile, components){
  INFO("=> Loading Plugin Servers")
  plugins <- listPlugins(config)
  plugin_profiles <- listPluginProfiles(config)
  if(length(plugin_profiles)>0){
    for(plugin_profile in plugin_profiles){
      plugin <- unlist(strsplit(unlist(strsplit(plugin_profile, paste0(dirname(plugin_profile),"/")))[2], ".json"))[1]
      plugin_def = plugins[[plugin]]
      outp <- loadModuleProfile(plugin_profile)
      enabled = outp$enabled
      if(!is.null(outp$restricted)) if(outp$restricted) {
        plugin_ok_with_roles <- any(sapply(profile$shiny_app_roles, function(x){x %in% outp$roles}))
        INFO("Plugin '%s' server %s (at least one of the profile roles [%s] do%s match plugin roles [%s]", 
             plugin, 
             ifelse(plugin_ok_with_roles, "enabled", "disabled"),
             paste0(unlist(profile$shiny_app_roles), collapse=","),
             ifelse(plugin_ok_with_roles, "", " NOT"),
             paste0(unlist(outp$roles), collapse=",")
        )
        enabled <- plugin_ok_with_roles
      }
      plugin_config = config$modules[[plugin]]
      has_config = !is.null(plugin_config)
      if(has_config) if(!is.null(plugin_config$enabled)) enabled = plugin_config$enabled
      if(enabled){
        INFO("Loading shiny plugin '%s' server functions...", plugin)
        if(is.null(plugin_def$server)) stop(sprintf("No UI defined for plugin!", plugin))
        eval_fun <- try(source(plugin_def$server), silent = TRUE)
        if(is(eval_fun, "try-error")) stop(sprintf("Something went wrong while sourcing server function for plugin '%s'!", plugin))
        plugin_server_fun <- eval_fun$value
        if(!is.null(plugin_server_fun)){
          if(!is(plugin_server_fun, "try-error")){
            #check formals
            check_module_server_formals(plugin, plugin_server_fun)
            #call server function
            called <- try(plugin_server_fun(plugin, parent.session = parent.session, config = config, profile = profile, components = components), silent = TRUE)
            if(is(called, "try-error")){
              ERROR("Error while calling shiny plugin '%s' server function", plugin)
            }
            INFO("Server function loaded for plugin '%s'", plugin)
          }else{
            ERROR("Error while evaluating server function for plugin '%s'", plugin)
          }
        }
      }
    }
  }else{
    INFO("No plugins defined in configuration")
  }
}

#loadPluginUIs
loadPluginUIs <- function(config = NULL, profile){
  INFO("=> Loading Plugin UIs")
  plugins <- listPlugins(config)
  plugin_profiles <- listPluginProfiles(config)
  module_uis <- lapply(plugin_profiles, function(plugin_profile){
    out <- NULL
    plugin <- unlist(strsplit(unlist(strsplit(plugin_profile, paste0(dirname(plugin_profile),"/")))[2], ".json"))[1]
    plugin_def = plugins[[plugin]]
    outp <- loadModuleProfile(plugin_profile)
    enabled = outp$enabled
    if(!is.null(outp$restricted)) if(outp$restricted){
      plugin_ok_with_roles <- any(sapply(profile$shiny_app_roles, function(x){x %in% outp$roles}))
      INFO("Plugin '%s' UI %s (at least one of the profile roles [%s] do%s match plugin roles [%s]", 
           plugin, 
           ifelse(plugin_ok_with_roles, "enabled", "disabled"),
           paste0(unlist(profile$shiny_app_roles), collapse=","),
           ifelse(plugin_ok_with_roles, "", " NOT"),
           paste0(unlist(outp$roles), collapse=",")
      )
      enabled <- plugin_ok_with_roles
    }
    plugin_config = config$modules[[plugin]]
    has_config = !is.null(plugin_config)
    if(has_config) if(!is.null(plugin_config$enabled)) enabled = plugin_config$enabled
    if(enabled){
      if(is.null(plugin_def$ui)) stop(sprintf("No UI defined for plugin!", plugin))
      eval_fun <- try(source(plugin_def$ui), silent = TRUE)
      if(is(eval_fun, "try-error")) stop(sprintf("Something went wrong while sourcing UI function for plugin '%s'!", plugin))
      plugin_ui_fun <- eval_fun$value
      #check formals
      check_module_ui_formals(plugin, plugin_ui_fun)
      #call ui function
      out <- plugin_ui_fun(plugin)
      if(is(out, "try-error")){
        out <- NULL
        ERROR("Error while calling shiny plugin '%s' UI function", plugin)
      }
      INFO("UI function loaded for plugin '%s'", plugin)
    }
    return(out)
  })
  module_uis <- module_uis[!sapply(module_uis, is.null)]
  return(module_uis)
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
    if(!is.null(outp$restricted)) if(outp$restricted) outp$enabled <- any(sapply(profile$shiny_app_roles, function(x){x %in% outp$roles}))
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
  
  #plugins
  if(length(config$plugins)>0){
    plugin_profiles <- lapply(names(config$plugins), function(plugin_name){
      plugin <- config$plugins[[plugin_name]]
      plugp <- loadModuleProfile(plugin$def)
      plugp$source <- plugin$def
      plugp$module <- plugin_name
      if(!is.null(plugp$restricted)) if(plugp$restricted) plugp$enabled <- any(sapply(profile$shiny_app_roles, function(x){x %in% plugp$roles}))
      return(plugp)
    })
    module_profiles <- c(module_profiles, plugin_profiles)
  }
  
  #default structure
  default_structure_menu_items <- module_profiles[sapply(module_profiles, function(x){x$type == "item"})]
  default_structure_menu_items <- default_structure_menu_items[order(sapply(default_structure_menu_items, function(x){x$rank}))]
  structure_menu_items <- default_structure_menu_items
  names(structure_menu_items) <- sapply(structure_menu_items, function(x){x$name})
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
      if(menu_item$name != "plugins") menuItem(text = menu_item$title, tabName = menu_item_name)
    }
  })))
}
