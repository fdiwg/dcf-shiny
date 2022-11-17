user_management_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      
      ns <- session$ns
      
      #User management (CRUD)
      model <- reactiveValues(
        users_to_add = 0,
        error = NULL
      )
      
      #user form
      showUserModal <- function(new = TRUE, id_user = NULL, username = NULL, fullname = NULL,
                                reporting_entities = NULL){
        print(reporting_entities)
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
            if(!is.null(config$dcf$reporting_entities)){
              #special case for country/flagstate (to display flag)
              if(config$dcf$reporting_entities$name %in% c("country", "flagstate")){
                selectizeInput(ns("user_form_reporting_entities"), label = "Reporting entities", selected = reporting_entities, multiple = TRUE, 
                  choices = {
                    ref_entity <- getReportingEntityCodes(config)
                    entity_choices <- ref_entity$code
                    setNames(entity_choices, ref_entity$label)
                  },options = list( 
                    render = I("{
                      item: function(item, escape) {
                        var icon_href = 'https://countryflagsapi.com/png/'+item.value.toLowerCase();
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      },
                      option: function(item, escape) { 
                        var icon_href = 'https://countryflagsapi.com/png/'+item.value.toLowerCase();
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      }
                    }"
                  ))
                )
              }else if(config$dcf$reporting_entities$name == "rfmo"){
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
      
      
      #list of users in DB
      #list of users that have been already administrated in the DCF database
      
      #dbuserTableHandler
      dbuserTableHandler <- function(data, uuids){
        
        if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              "User ID" = data[i,"id_user"],
              "User name" = data[i,"username"],
              "Full name" = data[i,"fullname"],
              "Reporting entities" = data[i,"reporting_entities"],
              Actions = as(actionButton(inputId = ns(paste0('button_edit_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                        title = "Save user", label = "", icon = icon("tasks")),"character")
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "User ID" = character(0),
            "User name" = character(0),
            "Full name" = character(0),
            "Reporting entities" = character(0),
            Actions = character(0)
          )
        }
        return(data)
      }
      
      #manage button handlers
      manageButtonEditEvents <- function(data, uuids){
        prefix <- paste0("button_edit_")
        if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          rep_entities <- unlist(strsplit(x$reporting_entities,","))
          if(length(rep_entities)==0) rep_entities = ""
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            showUserModal(
              new = FALSE, 
              id_user = x[,"id_user"],
              username = x[,"username"],
              fullname = x[,"fullname"],
              reporting_entities = rep_entities
            )
          })
        })
      }
      
      #renderDBUsers
      renderDBUsers <- function(data){
        
        uuids <- NULL
        if(!is.null(data)) if(nrow(data)>0) for(i in 1:nrow(data)){
          one_uuid = uuid::UUIDgenerate() 
          uuids <- c(uuids, one_uuid)
        }
        
        output$tbl_db_users <- DT::renderDT(
          dbuserTableHandler(data, uuids),
          selection='single', escape=FALSE,rownames=FALSE,
          options=list(
            lengthChange = FALSE,
            paging = FALSE,
            searching = FALSE,
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
        manageButtonEditEvents(data, uuids)
        
      }
      
      
      #delta (list of users in VRE - list of users in DB)
      #list all users that need to be administrated in the DCF database
      
      #getVREUsers
      #add filter on roles (when supported by API)
      getVREUsers <- function(db_users = NULL){
        out_users <- data.frame(
          username = character(0),
          fullname = character(0),
          stringsAsFactors = FALSE
        )
        out_vre <- httr::GET("https://api.d4science.org/rest/2/users/get-all-fullnames-and-usernames",
                             httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token)))
        if(httr::status_code(out_vre)==200){
          out_c <- content(out_vre)$result
          out_users <- data.frame(
            username = names(out_c),
            fullname = as.character(out_c),
            stringsAsFactors = FALSE
          ) 
          if(!is.null(db_users)){
            out_users <- out_users[sapply(out_users$username, function(x){!x %in% db_users$username}),]
          }
        }
        return(out_users)
      }
      
      #vreuserTableHandler
      vreuserTableHandler <- function(data, uuids){
        
        if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              "User name" = data[i,"username"],
              "Full name" = data[i,"fullname"],
              Actions = as(actionButton(inputId = ns(paste0('button_save_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                        title = "Save user to DB", label = "", icon = icon("tasks")),"character")
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "User name" = character(0),
            "Full name" = character(0),
            Actions = character(0)
          )
        }
        return(data)
      }
      
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
      
      #renderVREUsers
      renderVREUsers <- function(data){
        
        model$users_to_add <- nrow(data)
        
        uuids <- NULL
        if(!is.null(data)) if(nrow(data)>0) for(i in 1:nrow(data)){
          one_uuid = uuid::UUIDgenerate() 
          uuids <- c(uuids, one_uuid)
        }
        
        output$tbl_vre_users <- DT::renderDT(
          vreuserTableHandler(data, uuids),
          selection='single', escape=FALSE,rownames=FALSE,
          options=list(
            lengthChange = FALSE,
            paging = FALSE,
            searching = FALSE,
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
        
      }
      
      #renderAll
      renderAll <-function(pool){
        db_users <- as.data.frame(getDBUsers(pool))
        renderDBUsers(db_users)
        renderVREUsers(getVREUsers(db_users))
      }
      
      #user management observers
      observeEvent(input$user_add_go, {
        created <- createDBUser(
          pool = pool,
          profile = profile,
          username = input$user_form_username,
          fullname = input$user_form_fullname,
          reporting_entities = if(!is.null(config$dcf$reporting_entities)){input$user_form_reporting_entities}else{NULL}
        )
        if(created){
          model$error <- NULL
          removeModal()
          renderAll(pool)
        }else{
          model$error <- attr(created, "error")
        }
      })
      #data call/modify
      observeEvent(input$user_modify_go, {
        id_user <- ""
        updated <- updateDBUser(
          pool = pool,
          profile = profile,
          id_user = input$user_form_id,
          reporting_entities = if(!is.null(config$dcf$reporting_entities)){input$user_form_reporting_entities}else{NULL}
        )
        if(updated){
          model$error <- NULL
          removeModal()
          renderAll(pool)
        }else{
          model$error <- attr(updated, "error")
        }
      })
      
      #render tables
      observe({
        renderAll(pool)
      })
      
      output$user_tables <- renderUI({
        tabsetPanel(
          type = "pills",
          tabPanel("VRE New users", br(),
                   if(model$users_to_add==0){
                     tags$p("No new VRE user to manage in the database!", style = "color:green;font-weight:bold;")
                   }else if(model$users_to_add==1){
                     tags$p("There is 1 new VRE user to manage in the database!", style = "color:red;font-weight:bold;")
                   }else{
                     tags$p(sprintf("They are %s new VRE users to manage in the database!", model$users_to_add), style = "color:red;font-weight:bold;")
                   },
                   DT::dataTableOutput(ns("tbl_vre_users")) %>% withSpinner(type = 4)),
          tabPanel("DB Users", br(),
                   DT::dataTableOutput(ns("tbl_db_users")) %>% withSpinner(type = 4))
        )
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}