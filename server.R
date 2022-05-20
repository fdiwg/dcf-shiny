# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  #enrich profile with reporting entities
  PROFILE$reporting_entities <- getDBUserReportingEntities(profile = PROFILE, pool = POOL)
  
  #in case of expired data calls we automatically close them
  closeExpiredDataCalls(POOL) 
  
  #load module servers
  loadModuleServers(parent.session = session, config = CONFIG, profile = PROFILE, pool = POOL)
  
}