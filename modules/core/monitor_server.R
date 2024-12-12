monitor_server <- function(id, parent.session, config, profile, components,reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #machine
      output$machine_resources <- DT::renderDataTable({
          
          cpu = benchmarkme::get_cpu()
          ram = benchmarkme::get_ram()
          
          machine_df = as.data.frame(Sys.info())
          machine_df = data.frame(
            Property = row.names(machine_df),
            Value = machine_df$`Sys.info()`
          )
          machine_df = machine_df[!machine_df$Property %in% c("login", "user", "effective_user"),]
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
        },
        server = FALSE,
        rownames = FALSE,
        escape = FALSE
      )
      
      
      #D4science resources
      resources <- names(components)
      resources <- resources[!endsWith(resources,"_CONFIG")]
      
      output$software_resources <- DT::renderDataTable(
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

      output$resources_wrapper <- renderUI({
        
        shiny::tagList(
          box(title=HTML(paste0("<b>", as(shiny::icon("server"),"character"), " Machine Resources</b>")),collapsible = F,width=6, solidHeader = T, status = "info",
                DT::dataTableOutput(ns("machine_resources"))
          ),
          box(title=HTML(paste0("<b>", as(shiny::icon("database"),"character"), " Software Resources</b>")),collapsible = F,width=6, solidHeader = T, status = "info",
              DT::dataTableOutput(ns("software_resources"))
          ),
          if("admin" %in% profile$shiny_app_roles){
            box(title=HTML(paste0("<b>", as(shiny::icon("key"),"character"), " Token</b>")),collapsible = F,width=6, solidHeader = T, status = "info",
                div(
                  fluidRow(
                    column(6,
                      if(profile$expired){
                        tags$span(shiny::icon(c('times-circle')), "Token is expired", style="color:red;")
                      }else{
                        tags$span(shiny::icon(c('check-circle')), "Token is valid", style="color:green;")
                      }
                    )
                  ),
                  fluidRow(
                    column(5,shinyjs::disabled(passwordInput(ns("token"),label = NULL, value=profile$jwt))),
                    column(1,rclipButton(inputId = ns("clipbtn"),label = "Copy token",clipText = input$token,icon = icon("copy")))
                  )
                )
            )
          }else{
            NULL
          }
        )
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
  
}