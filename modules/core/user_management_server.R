user_management_server <- function(id, parent.session, config, profile, components, reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      
      ns <- session$ns
      
      #User management (CRUD)
      model <- reactiveValues(
        error = NULL
      )
      
      #user form
      showUserModal <- function(new = TRUE, id_user = NULL, username = NULL, fullname = NULL,
                                roles = NULL, reporting_entities = NULL){
        if(!is.null(roles)) if(roles[1]=="") roles <- NULL
        if(!is.null(reporting_entities)) if(reporting_entities[1]=="") reporting_entities <- NULL
        title_prefix <- ifelse(new, "Add", "Modify")
        form_action <- tolower(title_prefix)
        showModal(
          modalDialog(
            title = sprintf("%s user", title_prefix),
            if(!new){
              shinyjs::disabled(textInput(ns("user_form_id"), value = id_user, label = "User ID"))
            },
            shinyjs::disabled(textInput(ns("user_form_username"), value = username, label = "User name")),
            shinyjs::disabled(textInput(ns("user_form_fullname"), value = fullname, label = "Full name")),
            if(!is.null(config$dcf$roles)){
              pickerInput(ns("user_form_roles"), label = "Roles", selected = roles, multiple = TRUE,
                choices = getAllRoles(config)
              )
            },
            if(!is.null(config$dcf$reporting_entities)){
              #special case for country/flagstate (to display flag)
              if(config$dcf$reporting_entities$icon == "flag"){
                selectizeInput(ns("user_form_reporting_entities"), label = "Reporting entities", selected = reporting_entities, multiple = TRUE, 
                  choices = {
                    ref_entity <- getReportingEntityCodes(config)
                    entity_choices <- ref_entity$code
                    setNames(entity_choices, ref_entity$label)
                  },options = list( 
                    render = I("{
                      item: function(item, escape) {
                        var icon_href = 'https://raw.githubusercontent.com/fdiwg/flags/main/'+item.value.toLowerCase()+'.gif';
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      },
                      option: function(item, escape) { 
                        var icon_href = 'https://raw.githubusercontent.com/fdiwg/flags/main/'+item.value.toLowerCase()+'.gif';
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      }
                    }"
                  ))
                )
              }else if(config$dcf$reporting_entities$icon == "rfmo"){
                selectizeInput(ns("user_form_reporting_entities"), label = "Reporting entities", selected = reporting_entities, multiple = TRUE, 
                               choices = {
                                 ref_entity <- getReportingEntityCodes(config)
                                 entity_choices <- ref_entity$code
                                 setNames(entity_choices, ref_entity$label)
                               },options = list( 
                                 render = I("{
                      item: function(item, escape) {
                        var icon_href = 'https://www.fao.org/fishery/services/storage/fs/fishery/images/organization/logo/'+item.value.toLowerCase()+'.jpg';
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      },
                      option: function(item, escape) { 
                        var icon_href = 'https://www.fao.org/fishery/services/storage/fs/fishery/images/organization/logo/'+item.value.toLowerCase()+'.jpg';
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      }
                    }"
                  ))
                )
              }else{
                selectInput(ns("user_form_reporting_entities"), label = "Reporting entities", selected = reporting_entities, multiple = TRUE,
                  choices = {
                    ref_entity <- getReportingEntityCodes(config)
                    entity_choices <- ref_entity$code
                    setNames(entity_choices, ref_entity$label)
                  }
                )
              }
              
            },
            actionButton(ns(sprintf("user_%s_go", form_action)), title_prefix),
            uiOutput(ns("user_error")),
            easyClose = TRUE, footer = NULL
          )
        )
      }
      output$user_error <- renderUI({
        if(is.null(model$error)){
          tags$div(style="display:none;")
        }else{
          tags$div(model$error, class="alert alert-danger", role="alert")
        }
      })
      
  
      #userTableHandler
      userTableHandler <- function(data, uuids){
        
        if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            
            roles = unlist(strsplit(unlist(data[i,"roles"]),","))
            role_colors = sapply(roles, function(x){
              groups = names(config$dcf$groups)
              if(is.null(groups)) return(COLORS[1])
              parts = unlist(strsplit(x, ":"))
              if(length(parts)==1) return(COLORS[1])
              return(COLORS[which(groups == parts[1])+1])
            })
            reporting_entities = unlist(strsplit(unlist(data[i,"reporting_entities"]),","))
                           
            out_tib <- tibble::tibble(
              "User name" = data[i,"username"],
              "Full name" = data[i,"fullname"],
              "Registered in DB" = if(data[i,"db"]){as(icon("check"),"character")}else{""},
              "User ID" = data[i,"id_user"],
              "User roles" = paste0(sprintf("<span class='badge' style='background-color:%s'>%s</span>",role_colors, roles),collapse=" "),
              "Reporting entities" = paste0(sprintf("<span class='badge' style='background-color:%s'>%s</span>","gray", reporting_entities),collapse=" "),
              Actions = if(data[i,"id_user"]==""){
                as(actionButton(inputId = ns(paste0('button_save_', uuids[i])), class="btn btn-primary", style = "margin-right: 2px;",
                                title = "Save user to DB", label = "", icon = icon("tasks")),"character")
              }else{
                as(actionButton(inputId = ns(paste0('button_edit_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                title = "Edit DB user", label = "", icon = icon("pen")),"character")
              }
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "User name" = character(0),
            "Full name" = character(0),
            "Registered in DB" = character(0),
            "User ID" = character(0),
            "User roles" = character(0),
            "Reporting entities" = character(0),
            Actions = character(0)
          )
        }
        return(data)
      }
      
      
      #button handlers
      #manage button handlers
      manageButtonSaveEvents <- function(data, uuids){
        prefix <- paste0("button_save_")
        if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            showUserModal(
              new = TRUE, 
              username = x[,"username"],
              fullname = x[,"fullname"]
            )
          })
        })
      }
      #manage button handlers
      manageButtonEditEvents <- function(data, uuids){
        prefix <- paste0("button_edit_")
        if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          print(x)
          roles <- unlist(strsplit(as(x$roles,"character"), ","))
          rep_entities <- unlist(strsplit(as(x$reporting_entities,"character"),","))
          if(length(rep_entities)==0) rep_entities = ""
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            showUserModal(
              new = FALSE, 
              id_user = x[,"id_user"],
              username = x[,"username"],
              fullname = x[,"fullname"],
              roles = roles,
              reporting_entities = rep_entities
            )
          })
        })
      }
      
      #renderUsers
      renderUsers <- function(pool,profile){
        
        data <- getUsers(pool,profile)
        print(data)
        
        uuids <- NULL
        if(!is.null(data)) if(nrow(data)>0) for(i in 1:nrow(data)){
          one_uuid = uuid::UUIDgenerate() 
          uuids <- c(uuids, one_uuid)
        }
        output$tbl_users <- DT::renderDT(
          userTableHandler(data, uuids),
          selection='single', escape=FALSE,rownames=FALSE,
          options=list(
            lengthChange = TRUE,
            paging = TRUE,
            searching = TRUE,
            preDrawCallback = JS(
              'function() {
                  Shiny.unbindAll(this.api().table().node()); }'
            ),
            drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); }'
            ),
            autoWidth = FALSE,
            columnDefs = list(
              list(width = '100px', targets = c(0)),
              list(width = '400px', targets = c(1),
                   render = JS("function(data, type, full, meta) {
                           var html = data;
                           if(data.startsWith(\"http://\") | data.startsWith(\"https://\")){
                              html = '<a href=\"' + data + '\" target=\"_blank\">'+data+'</a>';
                           }
                           return html;
                        }"))
            )
          )
        )
        
        #manage action buttons
        manageButtonSaveEvents(data, uuids)
        manageButtonEditEvents(data, uuids)
      }
      
      #user management observers
      observeEvent(input$user_add_go, {
        waiting_screen<-tagList(
          h3("Creation in progress ..."),
          spin_flower(),
          h4(sprintf("Creation of user %s [%s]", input$user_form_fullname,input$user_form_username))
        )
        removeModal()
        waiter_show(html = waiting_screen, color = "#14141480")
        created <- createDBUser(
          pool = pool,
          profile = profile,
          username = input$user_form_username,
          fullname = input$user_form_fullname,
          roles = if(!is.null(config$dcf$roles)){input$user_form_roles}else{NULL},
          reporting_entities = if(!is.null(config$dcf$reporting_entities)){input$user_form_reporting_entities}else{NULL}
        )
        if(created){
          model$error <- NULL
          removeModal()
          renderUsers(pool,profile)
          waiter_hide()
        }else{
          model$error <- attr(created, "error")
        }
      })
      #data call/modify
      observeEvent(input$user_modify_go, {
        waiting_screen<-tagList(
          h3("Update in progress ..."),
          spin_flower(),
          h4(sprintf("Update of roles and reporting entities for user : %s [%s]", input$user_form_fullname,input$user_form_username))
        )
        removeModal()
        waiter_show(html = waiting_screen, color = "#14141480")
        id_user <- ""
        updated <- updateDBUser(
          pool = pool,
          profile = profile,
          id_user = input$user_form_id,
          roles = if(!is.null(config$dcf$roles)){input$user_form_roles}else{NULL},
          reporting_entities = if(!is.null(config$dcf$reporting_entities)){input$user_form_reporting_entities}else{NULL}
        )
        if(updated){
          model$error <- NULL
          removeModal()
          renderUsers(pool,profile)
          reloader <- reloader(id)
          waiter_hide()
        }else{
          model$error <- attr(updated, "error")
        }
      })
      
      #render tables
      observe({
        renderUsers(pool,profile)
      })
      
      output$user_table <- renderUI({
        shiny::tagList(
          h3("Users"),hr(),
          DT::dataTableOutput(ns("tbl_users")) %>% withSpinner(type = 4)
        )
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}