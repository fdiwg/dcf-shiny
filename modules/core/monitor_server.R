monitor_server <- function(id, parent.session, config, profile, pool){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #D4science resources
      resources <- c(
        "Database",
        "Workspace (StorageHub)", 
        "DataMiner (WPS)"
      )
      
      output$resources <- renderDataTable(
        data.frame(
          resource = resources,
          status = sapply(resources, function(resource){
            switch(resource,
               "Database" =  {
                 status <- !is(pool, "try-error")
                 if(status){
                   paste0(tags$span(shiny::icon("check-circle"), title = "Connected", style = "color:green;"), collapse="")
                 }else{
                   paste0(tags$span(shiny::icon("times-circle"), title = "Failed to connect", style = "color:red;"), collapse="")
                 }
               },
               "Workspace (StorageHub)" = {
                 status <- FALSE
                 if(!is.null(profile$access)){
                   STORAGEHUB <- try(d4storagehub4R::StoragehubManager$new(token = profile$access$access_token, token_type = "jwt"))
                   status <- !is(STORAGEHUB, "try-error")
                   status <- is(STORAGEHUB, "StoragehubManager")
                 }
                 if(status){
                   paste0(tags$span(shiny::icon("check-circle"), title = "Connected", style = "color:green;"), collapse="")
                 }else{
                   paste0(tags$span(shiny::icon("times-circle"), title = "Failed to connect", style = "color:red;"), collapse="")
                 }
                },
               
               "DataMiner (WPS)" = {
                 status <- FALSE
                 icproxy_req <- httr::GET(sprintf("https://registry.d4science.org/icproxy/gcube/service/ServiceEndpoint/DataAnalysis/DataMiner"),
                   httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token))
                 )
                 status <- httr::status_code(icproxy_req) == 200
                 if(status){
                   icproxy <- XML::xmlParse(httr::content(icproxy_req, "text"))
                   wps_uri = XML::xpathSApply(icproxy, "//AccessPoint/Interface/Endpoint", XML::xmlValue)[1]
                   if(!is.null(profile$access)){
                     WPS <- try(ows4R::WPSClient$new(
                       url = wps_uri, serviceVersion = "1.0.0",
                       headers = c("Authorization" = paste("Bearer", profile$access$access_token)),
                       logger = "DEBUG"
                     ))
                     status <- !is(WPS, "try-error")
                     status <- is(WPS, "WPSClient")
                   }else{
                     status <- FALSE
                   }
                 }
                 if(status){
                   paste0(tags$span(shiny::icon("check-circle"), title = "Connected", style = "color:green;"), collapse="")
                 }else{
                   paste0(tags$span(shiny::icon("times-circle"), title = "Failed to connect", style = "color:red;"), collapse="")
                 }
               }
            )
          })
        ),
        server = FALSE,
        rownames = FALSE,
        escape= FALSE
      )
      #-----------------------------------------------------------------------------------
    }
  )
  
}