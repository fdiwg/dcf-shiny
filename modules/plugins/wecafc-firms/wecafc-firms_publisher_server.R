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
      
      #combineData
      combineData <- function(newdata, dbdata){
        out <- rbind(newdata,dbdata)
        out$duplicated <- duplicated(out[,colnames(out)[!colnames(out) %in% c("measurement_value")]])
        out <- out[!out$duplicated,]
        out$duplicated <- NULL
        return(out)
      }
      
      #storeInDataspace
      storeInDataspace <- function(config, pool, profile, store, task_id, datafile){
        
        #check existance of task id folder, if not create it
        task_folder_id <- store$getWSItemID(parentFolderID = config$dataspace_id, itemPath = task_id)
        if(is.null(task_folder_id)){
          store$createFolder(folderID = config$dataspace_id, name = task_id)
          task_folder_id <- store$getWSItemID(parentFolderID = config$dataspace_id, itemPath = task_id)
        }
        
        #store data
        datafileID <- store$uploadFile(folderID = task_folder_id, file = datafile)
        return(datafileID)
      }
      
      #persistData
      persistData <- function(config, pool, profile, store, task_id){
        datetime <- format(Sys.time(),paste0("%Y%m%d%H%M%S"))
        table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
        
        #read data for latest open data call
        newdata <- readAndMergeAcceptedData(config, pool, profile, store, task_id)
        
        #store new data in dataspace
        newdata_file <- paste0(datetime, "_new.csv")
        readr::write_csv(newdata, newdata_file)
        storeInDataspace(config, pool, profile, store, task_id, newdata_file)
        unlink(newdata_file)
        
        #store db backup in dataspace
        dbdata <- NULL
        hasData <- DBI::dbExistsTable(pool, table_id, schema = "public")
        if(hasData){
          dbdata <- DBI::dbReadTable(pool, table_id, schema = "public")
          dbdata_file <- paste0(datetime, "_db_old.csv")
          readr::write_csv(dbdata, dbdata_file)
          storeInDataspace(config, pool, profile, store, task_id, dbdata_file)
          unlink(dbdata_file)
        }
        
        #combineData
        dbdatanew <- combineData(newdata, dbdata)
        
        #store db new version in workspace
        dbdatanew_file <- paste0(datetime, "_db_new.csv")
        readr::write_csv(dbdatanew, dbdatanew_file)
        storeInDataspace(config, pool, profile, store, task_id, dbdatanew_file)
        unlink(dbdatanew_file)
        
        #store new data in the database
        DBI::dbWriteTable(conn = pool, name = table_id, value = dbdatanew, overwrite = TRUE)
        
      }
      
      
      #-----------------------------------------------------------------------------------
    }
  )
}