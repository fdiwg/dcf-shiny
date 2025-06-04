data_call_server <- function(id, parent.session, config, profile, components, reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      
      ns <- session$ns
      
      #Data call management (CRUD)
      model <- reactiveValues(
        data_call_to_delete = NULL,
        error = NULL
      )
      
      onlyOpened<-reactiveVal(FALSE)
      
      #dcTableHandler
      dcTableHandler <- function(data, uuids){
        
        if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              "Data call ID" = data[i,"id_data_call"],
              "Task ID" = data[i,"task_id"],
              "Start date" = data[i,"date_start"],
              "End date" = data[i,"date_end"],
              "Status" = data[i,"status"],
              "Creator" = data[i,"creator_id"],
              "Creation date" = data[i, "creation_date"],
              "Updater" = data[i,"updater_id"],
              "Update date" = data[i,"update_date"],
              Actions = as(
                tagList(
                  actionButton(inputId = ns(paste0('button_edit_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                               title = "Edit data call", label = "", icon = icon("tasks")),
                  actionButton(inputId = ns(paste0('button_delete_', uuids[i])), class="btn btn-danger", style = "margin-right: 2px;",
                               title = "Delete data call", label = "", icon = icon("trash"))
                  ),  
              "character")
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "Data call ID" = character(0),
            "Task ID" = character(0),
            "Start date" = character(0),
            "End date" = character(0),
            "Status" = character(0), 
            "Creator" = character(0),
            "Creation date" = character(0),
            "Updater" = character(0),
            "Update date" = character(0),
            Actions = character(0)
          )
        }
        return(data)
      }
      
      #data call form
      showDataCallModal <- function(new = TRUE, id_data_call = NULL, task = "", start = Sys.Date(), end = NULL, status = "OPENED"){
        title_prefix <- ifelse(new, "Add", "Modify")
        form_action <- tolower(title_prefix)
        showModal(modalDialog(title = sprintf("%s data call", title_prefix),
                              if(new){
                                if(!is.null(config$dcf$groups)){
                                  shinyWidgets::pickerInput(ns("data_call_form_task"), label = "Task:", choices = getTasks(config, withId = T, profile = profile), selected = task)
                                }else{
                                  selectInput(ns("data_call_form_task"), "Task:",choices = getTasks(config, withId = T), selected = task)
                                }
                              }else{
                                shiny::tagList(
                                  shinyjs::disabled(textInput(ns("data_call_form_id"), value = id_data_call, label = "Data call ID")),
                                  shinyjs::disabled(
                                    if(!is.null(config$dcf$groups)){
                                      shinyWidgets::pickerInput(ns("data_call_form_task"), label = "Task:", choices = getTasks(config, withId = T, profile = profile), selected = task)
                                    }else{
                                      selectInput(ns("data_call_form_task"), "Task:",choices = getTasks(config), selected = task)
                                    }
                                  )
                                )
                              },
                              dateInput(ns("data_call_form_start"), "Start date", value = start),
                              dateInput(ns("data_call_form_end"), "End date", value = end),
                              selectInput(ns("data_call_form_status"), "Status", choices = list("OPENED", "CLOSED"), selected = "OPENED"),
                              actionButton(ns(sprintf("data_call_%s_go", form_action)), title_prefix),
                              actionButton(ns("data_call_cancel"), "Cancel", style = "float:right;"),
                              uiOutput(ns("data_call_error")),
                              easyClose = FALSE, footer = NULL ))
      }
      output$data_call_error <- renderUI({
        if(is.null(model$error)){
          tags$div(style="display:none;")
        }else{
          tags$div(model$error, class="alert alert-danger", role="alert")
        }
      })
      
      #manage button handlers
      manageButtonEditEvents <- function(data, uuids){
        prefix <- paste0("button_edit_")
        if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            showDataCallModal(
              new = FALSE,
              id_data_call = x[,"id_data_call"],
              task = x[,"task_id"],
              start = x[,"date_start"],
              end = x[,"date_end"],
              status = x[,"status"]
            )
          })
        })
      }
      
      manageButtonDeleteEvents <- function(data, uuids){
        prefix <- paste0("button_delete_")
        if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            model$data_call_to_delete <- x$id_data_call
            showModal(modalDialog(
              title = "Danger zone",
              tagList(
                p(sprintf("Do you really want to delete data call '%s' (Task %s, start on %s - end on %s) ?", x$id_data_call, x$task_id, x$date_start, x$date_end)),
                p("Click 'Yes' to confirm deletion or click 'Cancel'.")
              ),
              easyClose = F,
              footer = tagList(
                actionButton(ns("data_call_delete_go"),"Yes"),
                actionButton(ns("data_call_delete_cancel"),"Cancel")
              )
            ))
          })
        })
      }
      
      #renderDataCalls
      renderDataCalls <- function(data){
          
          model$error <- NULL
        
          uuids <- NULL
          if(!is.null(data)) if(nrow(data)>0) for(i in 1:nrow(data)){
            one_uuid = uuid::UUIDgenerate() 
            uuids <- c(uuids, one_uuid)
          }
          
          output$tbl_data_calls <- DT::renderDT(
            datatable(dcTableHandler(data, uuids),
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
            )%>% formatStyle(
            'Status',
            target = 'row',
            backgroundColor = styleEqual(c("OPENED","CLOSED"), c('#CEF3D6','#ffd6d6'))
          )
          )
          
          #manage action buttons
          manageButtonEditEvents(data, uuids)
          manageButtonDeleteEvents(data, uuids)
          
        }
      
      #render tables
      observe({
        renderDataCalls(getDataCalls(pool,tasks=input$task,status=onlyOpened()))
      })
      
      #status selector
      output$status_selector <- renderUI({
        checkboxInput(ns("limit_status"), "Only Opened", FALSE)
      })
      
      #Task selector
      output$task_selector<-renderUI({
        if(!is.null(config$dcf$groups)){
          shinyWidgets::pickerInput(ns("task"), 
                                    label = NULL, 
                                    multiple = T, 
                                    choices = getTasks(config, withId = T, profile = profile), 
                                    selected = NULL,
                                    options = list(
                                      placeholder = "Display tasks :"
                                    ))
        }else{
          selectizeInput(ns("task"),
                         label=NULL,
                         multiple = T,
                         choices = getTasks(config, withId=TRUE),
                         selected=NULL,
                         options = list(
                           placeholder = "Display tasks :"
                         )
          )
        }
      })
      
      observeEvent(input$limit_status,{
        req(!is.null(input$limit_status))
        if(input$limit_status){
          onlyOpened<-onlyOpened("OPENED")
        }else{
          onlyOpened<-onlyOpened(NULL)
        }
      }
      )
      
      
      #data call/add
      observeEvent(input$add_data_call,{
        showDataCallModal(new = TRUE)
      })
      observeEvent(input$data_call_add_go, {
        waiting_screen<-tagList(
          h3("Creation in progress ..."),
          spin_flower(),
          h4(sprintf("Creation of new data call for task '%s' [%s/%s]", input$data_call_form_task,as.character(input$data_call_form_start),as.character(input$data_call_form_end)))
        )
        removeModal()
        waiter_show(html = waiting_screen, color = "#14141480")
        created <- createDataCall(
          pool = pool,
          config = config,
          profile = profile,
          task = input$data_call_form_task,
          start = input$data_call_form_start,
          end = input$data_call_form_end,
          status = input$data_call_form_status
        )
        if(created){
          model$error <- NULL
          removeModal()
          renderDataCalls(getDataCalls(pool,tasks=input$task,status=onlyOpened()))
          reloader <- reloader(id)
          waiter_hide()
        }else{
          model$error <- attr(created, "error")
        }
        
      })
      #data call/modify
      observeEvent(input$data_call_modify_go, {
        waiting_screen<-tagList(
          h3("Modification in progress ..."),
          spin_flower(),
          h4(sprintf("Modification of data call '%s' for task '%s'", input$data_call_form_id,input$data_call_form_task))
        )
        removeModal()
        waiter_show(html = waiting_screen, color = "#14141480")
        id_call <- ""
        updated <- updateDataCall(
          pool = pool,
          profile = profile,
          config = config,
          id_data_call = input$data_call_form_id,
          task = input$data_call_form_task,
          start = input$data_call_form_start,
          end = input$data_call_form_end,
          status = input$data_call_form_status
        )
        if(updated){
          model$error <- NULL
          removeModal()
          renderDataCalls(getDataCalls(pool,tasks=input$task,status=onlyOpened()))
          reloader <- reloader(id)
          waiter_hide()
        }else{
          model$error <- attr(updated, "error")
        }
      })
      
      #data call/cancel
      observeEvent(input$data_call_cancel, {
        model$error <- NULL
        removeModal()
      })
      #data deletion
      #data call/modify
      observeEvent(input$data_call_delete_go, {
        waiting_screen<-tagList(
          h3("Deletion in progress ..."),
          spin_flower(),
          h4(sprintf("Deletion of data call '%s'", model$data_call_to_delete))
        )
        removeModal()
        waiter_show(html = waiting_screen, color = "#14141480")
        deleted <- deleteDataCall(
          pool = pool,
          config=config,
          profile=profile,
          id_data_call = model$data_call_to_delete
        )
        if(deleted){
          model$error <- NULL
          removeModal()
          renderDataCalls(getDataCalls(pool,tasks=input$task,status=onlyOpened()))
          reloader <- reloader(id)
          waiter_hide()
        }else{
          model$error <- attr(deleted, "error")
        }
        model$data_call_to_delete <- NULL
      })
      #data deletion/cancel
      observeEvent(input$data_call_delete_cancel,{
        model$data_call_to_delete <- NULL
        removeModal()
      })
      
      
      
      #-----------------------------------------------------------------------------------
    }
  )
}