# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  #enrich profile with reporting entities
  PROFILE$reporting_entities <- getDBUserReportingEntities(profile = PROFILE, pool = COMPONENTS$POOL)
  
  #in case of expired data calls we automatically close them
  closeExpiredDataCalls(COMPONENTS$POOL) 
  
  #load module servers
  loadModuleServers(parent.session = session, config = CONFIG, profile = PROFILE, components = COMPONENTS)
  
}