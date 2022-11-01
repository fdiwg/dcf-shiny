function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #store
      store <- components$STORAGEHUB
      pool <- components$POOL
      
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
      
      #readAndMergeAcceptedData (from latest open data call)
      readAndMergeAcceptedData <- function(config, pool, profile, store, task_id){
        datacalls <- getDataCalls(pool,tasks=task_id)
        datacalls <- datacalls[order(datacalls$date_end, datacalls$status,decreasing = T),][1,]
        data_submissions <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = FALSE,data_calls_id=datacalls$id_data_call,full_entities=TRUE)
        data_submissions <- data_submissions [data_submissions$status == "ACCEPTED",]
        newdata <- NULL
        if(nrow(data_submissions)>0){
          newdata <- do.call("rbind", lapply(1:nrow(data_submissions), function(i){
            data_submission <- data_submissions[i,]
            item <- store$getWSItem(parentFolderID = data_submission$id, itemPath = paste0(data_submission$data_call_folder, ".csv"))
            filename <- store$downloadItem(item = item, wd = tempdir())
            readr::read_csv(filename)
          }))
        }
        return(newdata)
      }
      
      #storeData
      storeData <- function(config, pool, profile, store, task_id){
        table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
        
        #read data for latest open data call
        newdata <- readAndMergeAcceptedData(config, pool, profile, store, task_id)
        
        #TODO store new data in a workspace for data manager
        #TODO store db backup in workspace
        #TODO store new data in the database
        hasData <- DBI::dbExistsTable(pool, table_id, schema = "public")
        if(hasData){
          #TODO data review/overwrite procedure
        }else{
          #TODO include data
        }
        
      }
      
      
      #-----------------------------------------------------------------------------------
    }
  )
}