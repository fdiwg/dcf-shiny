monitor_server <- function(input, output, session, profile, parent.session){
  
  ns <- session$ns
  
  resources <- c(
    "Workspace (StorageHub)", 
    "DataMiner (WPS)"
  )
  
  output$resources <- renderDataTable(
    data.frame(
      resource = resources,
      status = sapply(resources, function(resource){
        switch(resource,
               
           "Workspace (StorageHub)" = {
             status <- FALSE
             if(!is.null(PROFILE$access)){
               STORAGEHUB <- try(d4storagehub4R::StoragehubManager$new(token = PROFILE$access$access_token, token_type = "jwt"))
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
             icproxy_req <- httr::GET(sprintf("https://registry.d4science.org/icproxy/gcube/service//ServiceEndpoint/DataAnalysis/DataMiner?gcube-scope=%s", PROFILE$context))
             status <- httr::status_code(icproxy_req) == 200
             if(status){
               icproxy <- XML::xmlParse(httr::content(icproxy_req, "text"))
               wps_uri = xpathSApply(icproxy, "//AccessPoint/Interface/Endpoint", xmlValue)[1]
               if(!is.null(PROFILE$access)){
                 WPS <- try(ows4R::WPSClient$new(
                   url = wps_uri, serviceVersion = "1.0.0",
                   headers = c("Authorization" = paste("Bearer", PROFILE$access$access_token)),
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
  
}