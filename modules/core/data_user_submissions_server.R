data_user_submissions_server <- function(id, parent.session, config, profile, components,reloader){
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
      
      list_datacalls<-reactiveVal(NULL)
      datacall<-reactiveVal(NULL)
      first_datacall<-reactiveVal(TRUE)
      ready<-reactiveVal(TRUE)
      
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
          datacalls<-getDataCalls(pool,tasks=input$task,status="OPENED")
          datacalls<-datacalls[order(datacalls$date_end, datacalls$status,decreasing = T),]
          list_datacalls<-list_datacalls(datacalls)
          output$datacall_wrapper<-renderUI({
            selectizeInput(ns("datacall"),
                           label="Inspect your submissions for data call :",
                           multiple = F,
                           choices = setNames(datacalls$id_data_call,sprintf("%s (%s/%s)",datacalls$task_id,datacalls$date_start,datacalls$date_end)) ,
                           selected=NULL,
                           options = list(
                             placeholder = "Please select a datacall",
                             onInitialize = I('function() { this.setValue(""); }')
                           )
            )
          })
        }
        
      })
      
      output$run_wrapper<-renderUI({
        req(input$datacall)
        if(!is.null(input$datacall))if(input$datacall!=""){
          withBusyIndicatorUI(
            actionButton(ns("run"), "Display submissions")
          )
        }else{
          NULL
        }
      })
      
      output$data_submission_error <- renderUI({
        if(is.null(model$error)){
          tags$div(style="display:none;")
        }else{
          tags$div(model$error, class="alert alert-danger", role="alert")
        }
      })
      
      showSubmissionBrowseModal <- function(items){
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
      
      
      #modals
      showDeletionActionModal <- function(data_call_folder,data_call_id,task,reporting_entity){
        showModal(
          modalDialog(
            title = "",
            shinyjs::hidden(textInput(ns("data_submission_call_folder"), value = data_call_folder, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_data_call_id"), value =data_call_id, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_task"), value = task, label = "")),
            shinyjs::hidden(textInput(ns("data_submission_reporting_entity"), value =reporting_entity, label = "")),
            tags$p(sprintf("Are you sure to delete data submission '%s'?", data_call_folder)),
            actionButton(ns("submission_deletion_ok"), "Delete"),
            actionButton(ns("submission_deletion_cancel"), "Cancel", style = "float:right;"),
            uiOutput(ns("data_submission_error")),
            easyClose = FALSE, footer = NULL 
          )
        )
      }	  
      
      
      #manage button handlers
      
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
      manageButtonDeleteEvents <- function(data, uuids){
        prefix <- paste0("button_delete_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            shinyjs::disable(button_id)
            showDeletionActionModal(
              data_call_folder = x$data_call_folder,
              data_call_id=x$data_call_id,
              task=x$task_id,
              reporting_entity=x$reporting_entity
            )
            shinyjs::enable(button_id)
          })
        })
      }
      
      #submissionsTableHandler
      submissionsTableHandler <- function(data, uuids,config){
        if(length(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            item <- data[i,]
            out_tib <- tibble::tibble(
              "Submission ID" = item$id,
              "Data call ID" = item$data_call_id,
              "Task ID" = item$task_id,
              "Flag" = if(config$dcf$reporting_entities$name %in% c("country", "flagstate")){
                paste0('<img src="https://raw.githubusercontent.com/fdiwg/flags/main/', tolower(item$reporting_entity),'.gif" height=16 width=32></img>')
              }else if(config$dcf$reporting_entities$name == "rfmo"){
                paste0('<img src="https://www.fao.org/fishery/services/storage/fs/fishery/images/organization/logo/', tolower(item$reporting_entity),'.jpg" height=16 width=32></img>')
              }else{""},          "Reporting entity" = as.factor(item$reporting_entity),
              "Owner" = item$owner,
              "Creation time" = item$creationTime,
              "Last modified by" = item$lastModifiedBy,
              "Last modification time" = item$lastModificationTime,
              "Status" = as.factor(item$status),
              Actions = ifelse(item$status=="MISSING",as(
                          tagList()
                          ,"character"),as(
                          tagList(
                            actionButton(inputId = ns(paste0('button_browse_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                         title = "Browse data submission", label = "", icon = icon("eye")),
                            actionButton(inputId = ns(paste0('button_delete_', uuids[i])), class="btn btn-danger", style = "margin-right: 2px;",
                                         title = "Delete data submission", label = "", icon = icon("trash"))
                          ),"character")
                          )
                        )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "Submission ID" = character(0),
            "Data call ID" = character(0),
            "Task ID" = character(0),
            "Flag" = character(0),
            "Reporting entity" = character(0),
            "Owner" = character(0),
            "Creation time" = character(0),
            "Last modified by" = character(0),
            "Last modification time" = character(0),
            "Status" = character(0),
            Actions = character(0)
          )
        }
        
        if(config$dcf$reporting_entities$name %in% c("country", "flagstate")){
          
        }else if(config$dcf$reporting_entities$name == "rfmo"){
          data<-data%>%
            rename(Logo=Flag)
        }else{
          data<-data%>%
            select(-Flag)
        }
        
        return(data)
      }
      
      #renderBars
      renderBars<- function(data){
        
        nb_entities<-length(unique(data$reporting_entity))
        accepted_submissions<-length(unique(subset(data,status=="ACCEPTED")$reporting_entity))
        rejected_submissions<-length(unique(subset(data,status=="REJECTED")$reporting_entity))
        pending_submissions<-length(unique(subset(data,status=="SUBMITTED")$reporting_entity))
        transmitted_submissions<-length(unique(subset(data,status%in%c("ACCEPTED","SUBMITTED"))$reporting_entity))
        
        nb_missing<-length(unique(subset(data,status%in%c("MISSING","REJECTED")&!reporting_entity%in%subset(data,status%in%c("ACCEPTED","SUBMITTED"))$reporting_entity)$reporting_entity))
        nb_duplicate<-sum(duplicated(subset(data,status%in%c("ACCEPTED","SUBMITTED"),select=c(reporting_entity,data_call_folder))))
        
        time_remaining<-as.numeric(datacall()$date_end-Sys.Date(),unit="days")
        if(time_remaining<0){
          time_remaining="Expired"
        }else{
          time_remaining<-sprintf('%s days',time_remaining)
        }
        
        output$indicators<-renderUI({
          div(
            column(4,
                   progressInfoBox(title="Submissions", text=sprintf('%s/%s',transmitted_submissions,nb_entities),value=transmitted_submissions,description=paste0(round(transmitted_submissions/nb_entities*100,0),"% of completion"), max = nb_entities,icon = icon("share"),fill = TRUE, color = "yellow",width =12),
                   progressInfoBox(title="Accepted submissions", text=sprintf('%s/%s',accepted_submissions,nb_entities),value=accepted_submissions,description=paste0(round(accepted_submissions/nb_entities*100,0),"% of completion"), max = nb_entities,icon = icon("check"),fill = TRUE, color = "green",width =12)
            ),
            column(4,
                   infoBox("Pending submissions", pending_submissions, icon = icon("tasks"), fill = TRUE,color="orange",width = 12),
                   infoBox("Rejected submissions",rejected_submissions, icon = icon("exclamation-triangle"), fill = TRUE,color="purple",width = 12)
                   
            ),
            column(4,
                   infoBox("Missing submissions",nb_missing, icon = icon("bell"), fill = TRUE,color="red",width = 12),
                   infoBox("Days remaining",time_remaining, icon = icon("clock"), fill = TRUE,color="teal",width = 12)
            )
          )
          
        })
        
        
      }
      
      #renderSubmissions
      renderSubmissions <- function(data,config){

        uuids <- NULL
        if(length(data)>0) if(nrow(data)>0) for(i in 1:nrow(data)){
          one_uuid = uuid::UUIDgenerate() 
          uuids <- c(uuids, one_uuid)
        }
        
        output$tbl_my_submissions <- DT::renderDT({
          datatable(
          submissionsTableHandler(data, uuids,config),
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
            columnDefs = list(
              list(width = '100px', targets = c(0))
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
        manageButtonDeleteEvents(data, uuids)
        
      }
      
      output$refresh_wrapper<-renderUI({
        req(input$run)
        if(!is.null(input$datacall))if(input$datacall!=""){
          actionButton(ns("refresh"), "Refresh", icon = icon("refresh"))
        }else{
          NULL
        }
      })
      
      #events
      
      observeEvent(input$run,{
        req(input$datacall)
        if(!is.null(input$datacall))if(input$datacall!=""){
          if(!first_datacall()){
            first_datacall<-first_datacall(FALSE)
            ready<-ready(FALSE)
          }
          withBusyIndicatorServer(ns("run"), {
          datacalls<-list_datacalls()
          datacall<-datacall(subset(datacalls,id_data_call==input$datacall))
          data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = TRUE,data_calls_id=input$datacall,full_entities=TRUE)
          renderSubmissions(data,config)
          renderBars(data)
          ready<-ready(TRUE)
          })
        }
      })
      
      observeEvent(input$refresh,{
        req(input$datacall)
        if(!is.null(input$datacall))if(input$datacall!=""){
          INFO("submission table is refresh")
          data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = TRUE,data_calls_id=input$datacall,full_entities=TRUE)
          renderSubmissions(data,config)
          renderBars(data)
        }
      })
      
      observeEvent(input$submission_deletion_ok,{
        waiting_screen<-tagList(
          h3("Deletion of Submission"),
          spin_flower(),
          h4(sprintf("Deletion of '%s' Submission for task '%s'", input$data_submission_reporting_entity,input$data_submission_task))
        )
        removeModal()
        waiter_show(html = waiting_screen, color = "#14141480")
        deleted <- try(deleteSubmission(
          config = config, pool = pool, profile = profile, store = store,
          data_call_folder = input$data_submission_call_folder,
          data_call_id = input$data_submission_data_call_id,
          task=input$data_submission_task,
          reporting_entity=input$data_submission_reporting_entity
        ))
        
        if(!is(deleted, "try-error")){
          model$error <- NULL
          removeModal()
          data <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = TRUE,data_calls_id=input$datacall,full_entities=TRUE)
          renderSubmissions(data,config)
          renderBars(data)
          waiter_hide()
        }else{
          model$error <- "Unexpected error during submission deletion!"
        }
      })
      
      output$table_wrapper<-renderUI({
        req(input$run)
        if(!is.null(input$datacall))if(input$datacall!=""){
          withSpinner(DT::dataTableOutput(ns("tbl_my_submissions")), type = 4)
        }else{tags$span(shiny::icon(c('exclamation-triangle')), "No data call is currently selected", style="color:orange;")}
      })
      
      output$indicators_wrapper<-renderUI({
        req(input$run)
        if(!is.null(input$datacall))if(input$datacall!=""){
          if(ready()){
            withSpinner(uiOutput(ns("indicators")), type = 4)
          }else{tags$span("Reload")}
        }
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}
  