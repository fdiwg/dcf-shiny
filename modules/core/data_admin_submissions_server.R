data_admin_submissions_server <- function(id, parent.session, config, profile, components){
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
      
      #submissionsTableHandler
      submissionsTableHandler <- function(data, uuids){
        if(length(data)>0) if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            item <- data[i,]
            out_tib <- tibble::tibble(
              "Submission ID" = item$id,
              "Data call ID" = item$data_call_id,
              "Task ID" = item$task_id,
              "Reporting entity" = item$reporting_entity,
              "Owner" = item$owner,
              "Creation time" = item$creationTime,
              "Last modified by" = item$lastModifiedBy,
              "Last modification time" = item$lastModificationTime,
              "Status" = item$status,
              Actions = as(
                tagList(
                  actionButton(inputId = ns(paste0('button_browse_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                               title = "Browse data submission", label = "", icon = icon("tasks"))
                )
                ,"character")
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble(
            "Submission ID" = character(0),
            "Data call ID" = character(0),
            "Task ID" = character(0),
            "Reporting entity" = character(0),
            "Owner" = character(0),
            "Creation time" = character(0),
            "Last modified by" = character(0),
            "Last modification time" = character(0),
            "Status" = character(0),
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
        
        output$tbl_all_submissions <- DT::renderDT(
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
        
      }
      
      #refresh table each minute
      autoRefresh <- reactiveTimer(60000)
      
      #events
      observe({
        autoRefresh()
        INFO("submission table is refresh")
        data <- getSubmissions(config = config, store = store, user_only = FALSE)
        renderSubmissions(data)
      })
      
      observeEvent(input$refresh,{
        data <- getSubmissions(config = config, store = store, user_only = FALSE)
        renderSubmissions(data)
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}
