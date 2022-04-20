# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  loadModuleServers(parent.session = session, config = CONFIG, profile = PROFILE, pool = POOL)
  
}