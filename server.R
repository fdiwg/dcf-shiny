# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  #global variables / environment
  #---------------------------------------------------------------------------------------
  jwt <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")
  
  #D4S components
  #---------------------------------------------------------------------------------------
  PROFILE <- try(loadProfile(jwt))
  if(is(PROFILE, "try-error")){
    shiny::showModal(
      shiny::modalDialog(
        title = "Error with JWT token!",
        shiny::tagList(
          PROFILE,
          br(),
          sprintf("Token: %s", jwt)
        )
      )
    )
    stop("Application has stopped!")
  }
  
  
  COMPONENTS <- loadComponents(profile = PROFILE, sdi = FALSE)
  
  
  #TODO current config from file, next to get from Workspace URL inherited from ICPROXY
  #---------------------------------------------------------------------------------------
  #default config_file path for DEPLOYMENT (hidden file)
  config_file <- COMPONENTS$STORAGEHUB$downloadItemByPath("dcf-shiny-config/config.yml", wd = tempdir())
  
  #local configuration
  #If you are an R developer, you need to create a .REnviron file (no file extension) in /dcf-shiny dir
  #The file should include the local path for your shiny config file in that way:
  #DCF_SHINY_CONFIG=<your config path>
  local_config_file <- Sys.getenv("DCF_SHINY_CONFIG")
  if(nzchar(local_config_file)) config_file <- local_config_file
  CONFIG <- read_dcf_config(file = config_file)
  
  #DBI component to add
  #---------------------------------------------------------------------------------------
  pool <- loadDBI(config = CONFIG)
  COMPONENTS$POOL <- pool
  PROFILE <- fetchProfileRoles(pool = COMPONENTS$POOL, profile = PROFILE)
  if("admin" %in% PROFILE$shiny_app_roles){
    COMPONENTS <- loadComponents(profile = PROFILE, sdi = TRUE)
    COMPONENTS$POOL <- pool
  }
  
  #INITIALIZATION
  #---------------------------------------------------------------------------------------
  
  #initAppWorkspace
  CONFIG$dcf$user_workspace <- sprintf("%s-%s", CONFIG$dcf$workspace, PROFILE$preferred_username)
  CONFIG$workspace_id <- initAppWorkspace(config = CONFIG, profile = PROFILE, components = COMPONENTS)
  
  #initAppDataspace
  CONFIG$dataspace_id <- initAppDataspace(config = CONFIG, profile = PROFILE, components = COMPONENTS)
  
  #enrich profile with reporting entities
  PROFILE$reporting_entities <- getDBUserReportingEntities(profile = PROFILE, pool = COMPONENTS$POOL)
  
  #in case of expired data calls we automatically close them
  closeExpiredDataCalls(pool=COMPONENTS$POOL,config=CONFIG,profile = PROFILE) 
  
  #in case of started data calls we automatically open them
  openStartedDataCalls(pool=COMPONENTS$POOL,config=CONFIG,profile = PROFILE) 
  
  #load module servers
  loadModuleServers(parent.session = session, config = CONFIG, profile = PROFILE, components = COMPONENTS)
  loadPluginServers(parent.session = session, config = CONFIG, profile = PROFILE, components = COMPONENTS)
  
  #render UI
  output$header <- renderUI({
    tags$span(CONFIG$dcf$name)
  })
  output$sidebar <- renderUI({
    sidebarMenuFromModules(config = CONFIG, profile = PROFILE)
  })
  output$body <- renderUI({
    do.call("tabItems",c(
      loadModuleUIs(config = CONFIG, profile = PROFILE),
      loadPluginUIs(config = CONFIG, profile = PROFILE)
    ))
  })
  
  
}