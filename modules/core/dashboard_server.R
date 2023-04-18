dashboard_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
  
      pool <- components$POOL
      
      ns <- session$ns
      
      output$welcome <- renderUI({
        h2(sprintf("Welcome %s!", profile$name))
      })
      
      output$status <- renderUI({
        if(profile$expired){
          tags$span(shiny::icon(c('times-circle')), "Token is expired", style="color:red;")
        }else{
          tags$span(shiny::icon(c('check-circle')), "Token is valid", style="color:green;")
        }
      })
      
      output$token_wrapper <- renderUI({
        if("admin" %in% profile$shiny_app_roles){
          div(
            column(2,shinyjs::disabled(passwordInput(ns("token"), "Token:",value=profile$jwt))),
            column(1,rclipButton(inputId = ns("clipbtn"),label = "Copy token",clipText = input$token,icon = icon("copy")))
          )
        }else{
          tags$span("<hidden>")
        }
      })
      
      output$roles <- renderDataTable({
        datatable(
        data.frame(
          #roles = paste0(profile$shiny_app_roles, collapse=", ")
          role = paste0(sprintf("<span class='badge' style='background-color:%s'>%s</span>","gray",profile$shiny_app_roles),collapse=" ")
        ),escape=F)
      })
      #----------------------------------------------------------------------------------- 
    }
  )
  
}