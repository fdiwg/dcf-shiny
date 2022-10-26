data_admin_submissions_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      ns <- session$ns
      
      #store
      store <- components$STORAGEHUB
      pool <- components$POOL
      
      #reactives
      model <- reactiveValues(
        error = NULL
      )
      
      output$task_wrapper<-renderUI({
        selectizeInput(ns("task"),
                       label="Task",
                       multiple = F,
                       choices = getTasks(config,withId=TRUE),
                       selected=NULL,
                       options = list(
                         placeholder = "Select a task",
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        )
      })
      
      observeEvent(input$task,{
        req(input$task)
        if(!is.null(input$task))if(input$task!=""){
          datacalls<-getDataCalls(pool,tasks=input$task)
          datacalls<-datacalls[order(datacalls$date_end, datacalls$status,decreasing = T),]
          
          output$datacall_wrapper<-renderUI({
          selectizeInput(ns("datacall"),
                         label="Inspect submissions for data call :",
                         multiple = F,
                         choices = setNames(datacalls$id_data_call,sprintf("%s (%s/%s) [%s]",datacalls$task_id,datacalls$date_start,datacalls$date_end,datacalls$status)) ,
                         selected=NULL,
                         options = list(
                           placeholder = "Please select a datacall",
                           onInitialize = I('function() { this.setValue(""); }')
                         )
            )
          })
        }
      })
      
      output$data_submission_error <- renderUI({
        if(is.null(model$error)){
          tags$div(style="display:none;")
        }else{
          tags$div(model$error, class="alert alert-danger", role="alert")
        }
      })
      
      #modals
      showSubmissionActionModal <- function(data_call_folder, id_data_submission,task,submitter,end,data_call_id,reporting_entity,accept = FALSE){
        title_prefix <- ifelse(accept, "Accept", "Reject")
        form_action <- tolower(title_prefix)
        showModal(
          modalDialog(
            title = "",
            shinyjs::hidden(textInput(ns("data_submission_call_folder"), value = data_call_folder, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_id"), value = id_data_submission, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_task"), value = task, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_submitter"), value = submitter, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_end"), value =as(end,"character"), label = "")),
            shinyjs::hidden(textInput(ns("data_submission_data_call_id"), value =data_call_id, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_reporting_entity"), value =reporting_entity, label = "")),
            tags$p(sprintf("Are you sure to %s the data submission?", form_action)),
            textInput(ns("data_submission_comment"), value = "-", label = "Add a comment"),
            actionButton(ns(sprintf("data_submission_%s_go", form_action)), title_prefix),
            actionButton(ns("data_submission_cancel"), "Cancel", style = "float:right;"),
            uiOutput(ns("data_submission_error")),
            easyClose = FALSE, footer = NULL 
          )
        )
      }
      
      showSubmissionBrowseModal <- function(items){
        print(items)
        showModal(
          modalDialog(
            title = "",
            selectizeInput(ns("item"),
                           label="Show submitted item :",
                           multiple = F,
                           choices = setNames(items$name,items$description),
                           selected=NULL,
                           options = list(
                             placeholder = "Please select a item",
                             onInitialize = I('function() { this.setValue(""); }')
                           )
            ),
            uiOutput(ns("display")),
            easyClose = TRUE, footer = NULL,size="l" 
          )
        )
      }
      
      showReminderModal <- function(sended){
        showModal(
          modalDialog(
            title = "",
            p(
              if(!sended){
                attr(sended, "error")
              }else{
                "Your reminder has been successfully sent"
              }
            ),
            easyClose = TRUE, footer = NULL,size="l" 
          )
        )
      }
      
      #manage button handlers
      #Browse TODO
      manageButtonBrowseEvents <- function(data, uuids){
        prefix <- paste0("button_browse_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            shinyjs::disable(button_id)
            items<-copyItemsSubmission(store, data_submission_id=x$id, wd=tempdir())
             showSubmissionBrowseModal(
               items = items
             )
             shinyjs::enable(button_id)
          })
        })
      }
      
      #Browse
      manageButtonReminderEvents <- function(data, uuids){
        prefix <- paste0("button_reminder_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            shinyjs::disable(button_id)
            sended<-sendReminder(pool,data_call_id=x$data_call_id,reporting_entity=x$reporting_entity,role=config$dcf$roles$submitter,config, profile)
            showReminderModal(
               sended = sended
            )
          })
        })
      }
      
      #Accept
      manageButtonAcceptEvents <- function(data, uuids){
        prefix <- paste0("button_accept_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            shinyjs::disable(button_id)
            showSubmissionActionModal(
              data_call_folder = x$data_call_folder,
              id_data_submission = x$id,
              task=x$task_id,
              submitter=x$submitter,
              end=x$end,
              data_call_id=x$data_call_id,
              reporting_entity=x$reporting_entity,
              accept = TRUE
            )
            shinyjs::enable(button_id)
          })
        })
      }
      #Reject
      manageButtonRejectEvents <- function(data, uuids){
        prefix <- paste0("button_reject_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            shinyjs::disable(button_id)
            showSubmissionActionModal(
              data_call_folder = x$data_call_folder,
              id_data_submission = x$id,
              task=x$task_id,
              submitter=x$submitter,
              end=x$date_end,
              data_call_id=x$data_call_id,
              reporting_entity=x$reporting_entity,
              accept = FALSE
            )
            shinyjs::enable(button_id)
          })
        })
      }
    
    observeEvent(input$item, {
      req(input$item)
      if(endsWith(input$item,".pdf")){
        #data reports
        print("CLICK ON PDF")
        output$display <- renderUI({
          tags$iframe(style="height:600px; width:100%", src=paste0("tmp/",input$item))
        })
      }else if(endsWith(input$item,".csv")){
        #data files
        output$display_table <-DT::renderDT(
          readr::read_csv(file.path(tempdir(),input$item)),
          escape=FALSE,rownames=FALSE,
          options=list(
            pageLength = 5,
            searching = TRUE,
            autoWidth = FALSE,
            scrollX=TRUE,
            scrollCollapse=TRUE)
          )
        
        output$display<- renderUI({
          DT::dataTableOutput(ns("display_table"))
        })
      }else if(endsWith(input$item,".xml")){
        #metadata
        dcentry <- atom4R::readDCEntry(file.path(tempdir(),input$item))
        output$display_table <-DT::renderDT(
          dcentry$asDataFrame(),
          escape=FALSE,rownames=FALSE,
          options=list(
            pageLength = 10,
            searching = TRUE,
            autoWidth = FALSE,
            scrollX=TRUE,
            scrollCollapse=TRUE)
        )
        
        output$display<- renderUI({
          DT::dataTableOutput(ns("display_table"))
        })
      }else {
        #nothing in principle
        output$display<- renderUI({
          NULL
        })
      }
    })
      
      
      #observers on modals actions
      #data submission accept/cancel
      observeEvent(input$data_submission_accept_go, {
        accepted <- try(acceptSubmission(
          config = config,pool=pool,profile =profile, store = store,
          data_call_folder = input$data_submission_call_folder,
          data_submission_id = input$data_submission_id,
          task=input$data_submission_task,
          data_call_id=,input$data_submission_data_call_id,
          reporting_entity=input$data_submission_reporting_entity,
          username=input$data_submission_submitter,
          comment=input$data_submission_comment
        ))
        if(!is(accepted, "try-error")){
          if(!accepted){
            model$error <- attr(accepted,"error")
          }else{
            model$error <- NULL
            removeModal()
          }
          
          data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = FALSE,data_calls_id=input$datacall,full_entities=TRUE)
          renderSubmissions(data)
          renderBars(data)
        }else{
          model$error <- "Unexpected error during submission acceptance!"
        }
      })
      #data submission reject/cancel
      observeEvent(input$data_submission_reject_go, {
        rejected <- try(rejectSubmission(
          config = config, pool = pool, profile = profile, store = store,
          data_call_folder = input$data_submission_call_folder,
          data_submission_id = input$data_submission_id,
          task=input$data_submission_task,
          username=input$data_submission_submitter,
          end=input$data_submission_end,
          comment=input$data_submission_comment
        ))
        if(!is(rejected, "try-error")){
          model$error <- NULL
          removeModal()
          data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = FALSE,data_calls_id=input$datacall,full_entities=TRUE)
          renderSubmissions(data)
          renderBars(data)
        }else{
          model$error <- "Unexpected error during submission rejection!"
        }
      })
      #data submission status/cancel
      observeEvent(input$data_submission_cancel, {
        model$error <- NULL
        removeModal()
      })
      
      #submissionsTableHandler
      submissionsTableHandler <- function(data, uuids){
        if(length(data)>0) if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            item <- data[i,]
            out_tib <- tibble::tibble(
              "Rank" = ifelse(item$status=="ACCEPTED",1,
                              ifelse(item$status=="SUBMITTED",2,
                                     ifelse(item$status=="REJECTED",3,4))),
              "Submission ID" = item$id,
              "Data call ID" = item$data_call_id,
              "Data call Folder" = as.factor(item$data_call_folder),
              "Task ID" = as.factor(item$task_id),
              "Flag" = paste0('<img src="https://countryflagsapi.com/png/', tolower(item$reporting_entity),'" height=16 width=32></img>'),
              "Reporting entity" = as.factor(item$reporting_entity),
              "Temporal extent" = item$temporal_extent,
              "Submitter" = as.factor(item$submitter),
              "Creation time" = item$creationTime,
              "Last modified by" = item$lastModifiedBy,
              "Last modification time" = item$lastModificationTime,
              "Status" = as.factor(item$status),
              Actions = ifelse(item$status=="MISSING",as(
                          tagList(
                            actionButton(inputId = ns(paste0('button_reminder_', uuids[i])), class="btn btn-warning", style = "margin-right: 2px;",
                                        title = "Send a reminder", label = "", icon = icon("bell"))
                            ),"character"),
                        ifelse(item$status=="ACCEPTED",as(
                          tagList(
                            actionButton(inputId = ns(paste0('button_browse_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                         title = "Browse data submission", label = "", icon = icon("eye")),
                            actionButton(inputId = ns(paste0('button_reject_', uuids[i])), class="btn btn-danger", style = "margin-right: 2px;",
                                         title = "Reject data submission", label = "", icon = icon("remove")),
                            actionButton(inputId = ns(paste0('button_download_', uuids[i])), class="btn btn-default", style = "margin-right: 2px;",
                                         title = "Download data submission", label = "", icon = icon("download"))
                          ),"character"),
                        ifelse(item$status=="REJECTED",as(
                          tagList(
                            actionButton(inputId = ns(paste0('button_browse_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                         title = "Browse data submission", label = "", icon = icon("eye")),
                            actionButton(inputId = ns(paste0('button_accept_', uuids[i])), class="btn btn-success", style = "margin-right: 2px;",
                                         title = "Accept data submission", label = "", icon = icon("check")),
                            actionButton(inputId = ns(paste0('button_reminder_', uuids[i])), class="btn btn-warning", style = "margin-right: 2px;",
                                         title = "Send a reminder", label = "", icon = icon("bell")),
                            actionButton(inputId = ns(paste0('button_download_', uuids[i])), class="btn btn-default", style = "margin-right: 2px;",
                                         title = "Download data submission", label = "", icon = icon("download"))
                          ),"character"),  as(
                          tagList(
                            actionButton(inputId = ns(paste0('button_browse_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                         title = "Browse data submission", label = "", icon = icon("eye")),
                            actionButton(inputId = ns(paste0('button_accept_', uuids[i])), class="btn btn-success", style = "margin-right: 2px;",
                                         title = "Accept data submission", label = "", icon = icon("check")),
                            actionButton(inputId = ns(paste0('button_reject_', uuids[i])), class="btn btn-danger", style = "margin-right: 2px;",
                                         title = "Reject data submission", label = "", icon = icon("remove")),
                            actionButton(inputId = ns(paste0('button_download_', uuids[i])), class="btn btn-default", style = "margin-right: 2px;",
                                         title = "Download data submission", label = "", icon = icon("download"))
                          ),"character")
                )))
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble(
            "Rank" = character(0),
            "Submission ID" = character(0),
            "Data call ID" = character(0),
            "Data call Folder" = character(0),
            "Task ID" = character(0),
            "Flag" = character(0),
            "Reporting entity" = character(0),
            "Temporal extent" = character(0),
            "Submitter" = character(0),
            "Creation time" = character(0),
            "Last modified by" = character(0),
            "Last modification time" = character(0),
            "Status" = character(0),
            Actions = character(0)
          )
        }
        return(data)
      }
      
      #renderBars
      renderBars<- function(data){
        
        nb_entities<-length(unique(data$reporting_entity))
        accepted_submissions<-length(unique(subset(data,status=="ACCEPTED")$reporting_entity))
        pending_submissions<-length(unique(subset(data,status=="SUBMITTED")$reporting_entity))
        transmitted_submissions<-length(unique(subset(data,status%in%c("ACCEPTED","SUBMITTED"))$reporting_entity))
        nb_missing<-length(unique(subset(data,status%in%c("MISSING","REJECTED"))$reporting_entity))
        nb_duplicate<-sum(duplicated(subset(data,status%in%c("ACCEPTED","SUBMITTED"),select=c(reporting_entity,data_call_folder))))
        
      output$indicators<-renderUI({
        div(
            progressInfoBox(title="Submission", text=sprintf('%s/%s',transmitted_submissions,nb_entities),value=transmitted_submissions,description=paste0(transmitted_submissions/nb_entities*100,"% of completion"), max = nb_entities,icon = icon("share"),fill = TRUE, color = "yellow",width =2),
            progressInfoBox(title="Accepted submissions", text=sprintf('%s/%s',accepted_submissions,nb_entities),value=accepted_submissions,description=paste0(accepted_submissions/nb_entities*100,"% of completion"), max = nb_entities,icon = icon("check"),fill = TRUE, color = "green",width =2),
            infoBox("Pending submissions", pending_submissions, icon = icon("tasks"), fill = TRUE,color="orange",width = 2),
            infoBox("Missing submissions",nb_missing, icon = icon("bell"), fill = TRUE,color="red",width = 2),
            infoBox("Duplicate submissions",nb_duplicate, icon = icon("exclamation-triangle"), fill = TRUE,color="purple",width = 2),
        )
      })

      
      }
      
      #renderSubmissions
      renderSubmissions <- function(data){
        
        uuids <- NULL
        if(length(data)>0) if(nrow(data)>0) for(i in 1:nrow(data)){
          one_uuid = uuid::UUIDgenerate() 
          uuids <- c(uuids, one_uuid)
        }
        
        output$tbl_all_submissions <- DT::renderDT({
          datatable(
          submissionsTableHandler(data, uuids),
          selection='single', escape=FALSE,rownames=FALSE,filter = list(position = 'top', clear = FALSE),
          options=list(
            lengthChange = FALSE,
            paging = FALSE,
            searching = TRUE,
            preDrawCallback = JS(
              'function() {
                  Shiny.unbindAll(this.api().table().node()); }'
            ),
            drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); }'
            ),
            autoWidth = FALSE,
            scrollY="600px",
            scrollCollapse=TRUE,
            order = list(list(0,'asc')),
            columnDefs = list(
              list(width = '100px', targets = c(1)),
              list(visible=FALSE, targets=c(0))
            )
          )
          )%>% formatStyle(
            'Status',
            target = 'row',
            backgroundColor = styleEqual(c("MISSING","SUBMITTED","ACCEPTED","REJECTED"), c('#ffd6d6', '#FFE4AD','#CEF3D6','#F3A5A8'))
          )
        })
        
        #manage action buttons
        manageButtonBrowseEvents(data, uuids)
        manageButtonReminderEvents(data, uuids)
        manageButtonAcceptEvents(data, uuids)
        manageButtonRejectEvents(data, uuids)
        
      }
      
      output$refresh_wrapper<-renderUI({
        req(input$datacall)
        if(!is.null(input$datacall))if(input$datacall!=""){
        actionButton(ns("refresh"), "Refresh", icon = icon("refresh"))
        }else{
          NULL
        }
      })
      
      #events
      observeEvent(input$datacall,{
        req(input$datacall)
        if(!is.null(input$datacall))if(input$datacall!=""){
        data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = FALSE,data_calls_id=input$datacall,full_entities=TRUE)
        renderSubmissions(data)
        renderBars(data)
        }
      })
      
      observeEvent(input$refresh,{
        req(input$datacall)
        if(!is.null(input$datacall))if(input$datacall!=""){
        data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = FALSE,data_calls_id=input$datacall,full_entities=TRUE)
        renderSubmissions(data)
        renderBars(data)
        INFO("all submissions table is refresh")
        }
      })
      
      output$table_wrapper<-renderUI({
        if(!is.null(input$datacall))if(input$datacall!=""){
          withSpinner(DT::dataTableOutput(ns("tbl_all_submissions")), type = 4)
        }else{tags$span(shiny::icon(c('exclamation-triangle')), "No data call is currently selected", style="color:orange;")}
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}
