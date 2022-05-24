data_submissions_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      ns <- session$ns
      
      #store
      store <- components$STORAGEHUB
      
      #reactives
      selection <- reactiveVal(NULL)
      
      #manage button handlers
      manageButtonBrowseEvents <- function(data, uuids){
        prefix <- paste0("button_browse_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            folder_name = x$title
            selection(folder_name)
          })
        })
      }
      manageButtonDeleteEvents <- function(data, uuids){
        prefix <- paste0("button_delete_")
        if(length(data)>0) if(nrow(data)>0) lapply(1:nrow(data),function(i){
          x <- data[i,]
          button_id <- paste0(prefix,uuids[i])
          observeEvent(input[[button_id]],{
            folder_name = x$title
            selection(folder_name)
            showModal(modalDialog(
              title = "Danger zone",
              p(sprintf("Are you sure to delete data submission '%s'?", folder_name)),
              actionButton(ns("submission_deletion_cancel"), "Cancel"),
              actionButton(ns("submission_deletion_ok"), "Delete"),
              easyClose = FALSE,
              icon = icon("exclamation-triangle"),
              footer = NULL 
            ))
          })
        })
      }
      
      #submissionsTableHandler
      submissionsTableHandler <- function(data, uuids){
        if(length(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            
            data_call_folder <- basename(data[i,]$path)
            data_call_props <- unlist(strsplit(data_call_folder, "_for_"))
            reporting_entity <- data_call_props[2]
            data_call_props <- unlist(strsplit(data_call_props[1],"_"))
            data_call_id <- unlist(strsplit(data_call_props[1], "datacall-"))[2]
            task_id <- unlist(strsplit(data_call_props[2], "task-"))
            task_id <- paste0(task_id[2:length(task_id)], collapse = "task-")
            
            out_tib <- tibble::tibble(
              "Data call ID" = data_call_id,
              "Task ID" = task_id,
              "Reporting entity" = reporting_entity,
              Actions = as(
                tagList(
                  actionButton(inputId = ns(paste0('button_browse_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                               title = "Browse data submission", label = "", icon = icon("tasks")),
                  actionButton(inputId = ns(paste0('button_delete_', uuids[i])), class="btn btn-danger", style = "margin-right: 2px;",
                               title = "Delete data submission", label = "", icon = icon("trash"))
                )
                ,"character")
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "Data call ID" = character(0),
            "Task ID" = character(0),
            "Reporting entity" = character(0),
            Actions = character(0)
          )
        }
        return(data)
      }
      
      #renderSubmissions
      renderSubmissions <- function(data){

        uuids <- NULL
        if(length(data)>0) if(nrow(data)>0) for(i in 1:nrow(data)){
          one_uuid = uuid::UUIDgenerate() 
          uuids <- c(uuids, one_uuid)
        }
        
        output$tbl_data_submissions <- DT::renderDT(
          submissionsTableHandler(data, uuids),
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
        manageButtonBrowseEvents(data, uuids)
        manageButtonDeleteEvents(data, uuids)
        
      }
      
      #refresh table each minute
      autoRefresh <- reactiveTimer(60000)
      
      #events
      
      observe({
        autoRefresh()
        INFO("submission table is refresh")
        data <- store$listWSItemsByPath(folderPath = config$dcf$workspace)
        renderSubmissions(data)
      })
      
      observeEvent(input$refresh,{
        data <- store$listWSItemsByPath(folderPath = config$dcf$workspace)
        renderSubmissions(data)
      })
      
      observeEvent(input$submission_deletion_ok,{
        INFO("Delete '%s'", file.path(config$dcf$workspace, selection()))
        unshared <- store$unshareItem(itemPath = file.path(config$dcf$workspace, selection()), users = "emmanuel.blondel")
        deleted <- FALSE
        if(unshared){
          deleted <- store$deleteItem(itemPath = file.path(config$dcf$workspace, selection()))
        }else{
          print("Ups 1")
          #TODO ups something went wrong when trying to unshare
        }
        if(deleted){
          removeModal()
          data <- store$listWSItemsByPath(folderPath = config$dcf$workspace)
          renderSubmissions(data)
        }else{
          print("Ups 2")
          #TODO ups seomthing went wrong when trying to delete
        }
      })
      observeEvent(input$submission_deletion_cancel,{
        selection(NULL)
        removeModal()
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}
  