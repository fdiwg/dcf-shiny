monitor_server <- function(id, parent.session, config, profile, components,reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #machine
      output$machine_resources <- renderDataTable({
        
        cpu = benchmarkme::get_cpu()
        ram = benchmarkme::get_ram()
        
        machine_df = as.data.frame(Sys.info())
        machine_df = data.frame(
          Property = row.names(machine_df),
          Value = machine_df$`Sys.info()`
        )
        machine_df = do.call("rbind", list(
          machine_df,
          data.frame(
            Property = paste0("cpu_", names(cpu)), 
            Value = unlist(cpu)
          ),
          data.frame(
            Property = "ram",
            Value = paste(round(as(ram, "numeric")/1e9, 1), "GB")
          ),
          data.frame(
            Property = "cores",
            Value = parallel::detectCores()
          )
        ))
        row.names(machine_df) = 1:nrow(machine_df)
        machine_df
      })
      
      
      #D4science resources
      resources <- names(components)
      resources <- resources[!endsWith(resources,"_CONFIG")]
      
      output$software_resources <- renderDataTable(
        do.call("rbind", lapply(resources, function(resource_name){
          resource <- components[[resource_name]]
          data.frame(
            name = resource_name, 
            description = attr(resource, "description"), 
            status = {
              if(is(resource, "try-error")){
                paste0(tags$span(shiny::icon("times-circle"), title = "Failed to connect", style = "color:red;", "Failed to connect"), collapse="")
              }else if(is(resource, "unavailable")){
                paste0(tags$span(shiny::icon("times-circle"), title = "Unavailable", style = "color:black;", "Unavailable"), collapse="")
              }else{
                paste0(tags$span(shiny::icon("check-circle"), title = "Connected", style = "color:green;", "Connected"), collapse="")
              }
            }
          )
        })),
        server = FALSE,
        rownames = FALSE,
        escape= FALSE
      )
      
      output$status <- renderUI({
        if(profile$expired){
          tags$span(shiny::icon(c('times-circle')), "Token is expired", style="color:red;")
        }else{
          tags$span(shiny::icon(c('check-circle')), "Token is valid", style="color:green;")
        }
      })
      
      output$machine_resources_wrapper <- renderUI({
          box(title=HTML("<b>Machine Resources</b>"),collapsible = T,width=12,
                DT::dataTableOutput(ns("machine_resources"))
          )
      })
      
      output$software_resources_wrapper <- renderUI({
        box(title=HTML("<b>Software Resources</b>"),collapsible = T,width=12,
            DT::dataTableOutput(ns("software_resources"))
        )
      })
            
      output$token_wrapper <- renderUI({
        
        if("admin" %in% profile$shiny_app_roles){
          box(title=HTML("<b>Token</b>"),collapsible = T,width=12,
              div(
                uiOutput(ns("status")),
                column(2,shinyjs::disabled(passwordInput(ns("token"), "Token:",value=profile$jwt))),
                column(1,rclipButton(inputId = ns("clipbtn"),label = "Copy token",clipText = input$token,icon = icon("copy")))
              )
          )
        }else{
          NULL
        }
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
  
}