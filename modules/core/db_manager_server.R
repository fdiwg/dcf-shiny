db_manager_server <- function(id, parent.session, config, profile, components,reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      restart<-reactiveVal(TRUE)
      data_resources <- reactiveValues(
        task = NULL,
        newdata = NULL,
        dbdata = NULL,
        dbdatanew_duplicates = NULL,
        dbdatanew_noduplicates = NULL
      )
      
      #store
      store <- components$STORAGEHUB
      pool <- components$POOL
      
      #getDataTaskTablename
      getDataTaskTablename <- function(task_id){
        table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
        return(table_id)
      }
      
      #readAndMergeAcceptedData (from latest open data call)
      readAndMergeAcceptedData <- function(config, pool, profile, store, task_id){
        datacalls <- getDataCalls(pool,tasks=task_id)
        datacalls <- datacalls[order(datacalls$date_end, datacalls$status,decreasing = T),][1,]
        data_submissions <- getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only = FALSE,data_calls_id=datacalls$id_data_call,full_entities=TRUE)
        data_submissions <- data_submissions [data_submissions$status == "ACCEPTED",]
        newdata <- NULL
        if(nrow(data_submissions)>0){
          newdata <- do.call(plyr::rbind.fill, lapply(1:nrow(data_submissions), function(i){
            data_submission <- data_submissions[i,]
            item_list = store$listWSItems(parentFolderID = data_submission$id)
            data_is_zipped = isTRUE(config$storagehub$upload_zip)
            item <- item_list[item_list$name == paste0(data_submission$data_call_folder, ".", ifelse(data_is_zipped, "zip", "csv")),]
            tmp_wd = tempdir()
            filename <- store$downloadItem(item = item, wd = tmp_wd)
            if(data_is_zipped){
              csvfile = basename(gsub("\\.zip", ".csv", filename))
              zip::unzip(zipfile = filename, files = csvfile, exdir = tmp_wd)
              filename = file.path(tmp_wd, csvfile)
            }
            data <- readr::read_csv(filename, col_types = list(measurement_unit = readr::col_character()))
            
          }))
        }
        return(newdata)
      }
      
      #getDataTaskDBData
      getDataTaskDBData <- function(pool, task_id){
        out <- NULL
        table_id <- getDataTaskTablename(task_id)
        hasData <- DBI::dbExistsTable(pool, table_id, schema = "public")
        if(hasData){
          out <- DBI::dbReadTable(pool, table_id, schema = "public")
        }
        return(out)
      }
      
      #combineData
      combineData <- function(newdata, dbdata, show_duplicates = FALSE){
        if(!is.null(newdata)) newdata <- cbind(source = "new", newdata)
        if(!is.null(dbdata)) dbdata <- cbind(source = "database", dbdata)
        if(is.null(dbdata)){
          if(show_duplicates){
            return(cbind(newdata, duplicated = FALSE))
          }else{
            newdata$source <- NULL
            return(newdata)
          }
        }
        out <- plyr::rbind.fill(newdata,dbdata)
        out$duplicated <- duplicated(out[,colnames(out)[!colnames(out) %in% c("source", "measurement_value")]])
        if(!show_duplicates){
          out <- out[!out$duplicated,]
          out$duplicated <- NULL
          out$source <- NULL
        }
        return(out)
      }
      
      #storeInDataspace
      storeInDataspace <- function(config, pool, profile, store, task_id, datafile){
        
        #check existence of task id folder, if not create it
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
      persistData <- function(session, config, pool, profile, store){
        
        progress <- shiny::Progress$new(session, min = 0, max = 5)
        on.exit(progress$close())
        
        task_id <- data_resources$task
        INFO("Persisting data for task '%s'", task_id)
        datetime <- format(Sys.time(),paste0("%Y%m%d%H%M%S"))
        
        #store new data in dataspace
        newdata_file <- paste0(datetime, "_new.csv")
        progress$set(value = 1, message = sprintf("Store new data in dataspace: file '%s'", newdata_file))
        readr::write_csv(data_resources$newdata, newdata_file)
        #if upload_zip enabled we zip the CSV
        if(isTRUE(config$storagehub$upload_zip)){
          newdata_file_zip = gsub("\\.csv", ".zip", newdata_file)
          zip::zipr(zipfile = newdata_file_zip, files = newdata_file)
          unlink(newdata_file)
          newdata_file = newdata_file_zip
        }
        storeInDataspace(config, pool, profile, store, task_id, newdata_file)
        unlink(newdata_file)
        if(isTRUE(config$storagehub$upload_zip)) unlink(newdata_file_zip)
        
        #store db backup in dataspace
        if(!is.null(data_resources$dbdata)){
          dbdata_file <- paste0(datetime, "_db_old.csv")
          progress$set(value = 2, message = sprintf("Store DB data dump in dataspace: file '%s'", dbdata_file))
          readr::write_csv(data_resources$dbdata, dbdata_file)
          #if upload_zip enabled we zip the CSV
          if(isTRUE(config$storagehub$upload_zip)){
            dbdata_file_zip = gsub("\\.csv", ".zip", dbdata_file)
            zip::zipr(zipfile = dbdata_file_zip, files = dbdata_file)
            unlink(dbdata_file)
            dbdata_file = dbdata_file_zip
          }
          storeInDataspace(config, pool, profile, store, task_id, dbdata_file)
          unlink(dbdata_file)
        }
        
        #store db new version in workspace
        dbdatanew_file <- paste0(datetime, "_db_new.csv")
        progress$set(value = 3, message = sprintf("Store DB updated data in dataspace: file '%s'", dbdatanew_file))
        readr::write_csv(data_resources$dbdatanew_noduplicates, dbdatanew_file)
        #if upload_zip enabled we zip the CSV
        if(isTRUE(config$storagehub$upload_zip)){
          dbdatanew_file_zip = gsub("\\.csv", ".zip", dbdatanew_file)
          zip::zipr(zipfile = dbdatanew_file_zip, files = dbdatanew_file)
          unlink(dbdatanew_file)
          dbdatanew_file = dbdatanew_file_zip
        }
        storeInDataspace(config, pool, profile, store, task_id, dbdatanew_file)
        unlink(dbdatanew_file)
        
        #store new data in the database
        table_id <- getDataTaskTablename(task_id)
        progress$set(value = 4, message = sprintf("Write data in database table '%s'", table_id))
        DBI::dbWriteTable(conn = pool, name = table_id, value = data_resources$dbdatanew_noduplicates, overwrite = TRUE)
        if(!is.null(config$dbi$readonly_user)){
          #table is new or overwriten, need to grant to readonly user
          INFO("Granting SELECT privilege on data task table '%s' for readonly user '%s'", table_id, config$dbi$readonly_user)
          DBI::dbExecute(
            conn = pool, 
            statement = sprintf("GRANT SELECT ON %s TO %s WITH GRANT OPTION;", 
                                table_id, config$dbi$readonly_user)
          )
        }else{
          WARN("Granting SELECT privilege on data task table '%s' is skipped. No readonly user defined in config.", table_id)
        }
        progress$set(value = 5, message = "Data successfully persisted!")
      }
      
      #Wizard panels routine
      #-----------------------------------------------------------------------------------
      observeEvent(restart(),{
        req(isTRUE(restart()))
        output$wizard<-renderUI({
          
          tags$div(class ="row",
                   tags$div(class = "col-md-12",
                            tagList(     
                              tags$div(class = "connecting-line"),
                              tabsetPanel(id = "db-wizard-tabs",
                                          type="pills",
                                          tabPanel(title="Database manager",
                                                   value="db_home",
                                                   select = TRUE,
                                                   h2("Welcome to the database manager"),
                                                   p("Within this module you you will be able to:"),
                                                   tags$ul(
                                                     tags$li("Merge accepted data submissions for defined data dasks"),
                                                     tags$li("Check merged data and store it into the database")
                                                   ),
                                                   p("If you are ready, click on 'Start'"),
                                                   actionButton(ns("start"),"Start")
                                          )
                              )
                            )
                   )
          )
          
        })
      })
      
      #TAB 1 - MERGE DATA
      #TAB 1 MANAGER
      #----------------------
      observeEvent(input$start,{
        restart<-restart(FALSE)
        appendTab(inputId = "db-wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="1-Merge data", 
                    value ="db_merge_data",
                    tagList(
                      br(),
                      uiOutput(ns("task_wrapper")),
                      uiOutput(ns("go_readandmerge_wrapper"))
                    )
                  )
        )
        removeTab(inputId = "db-wizard-tabs", 
                  session = parent.session,
                  target = "db_home")
        updateTabsetPanel(inputId = "db-wizard-tabs", 
                          session = parent.session,
                          selected = "db_merge_data")
      })
      
      #task_wrapper
      output$task_wrapper<-renderUI({
        if(!is.null(config$dcf$groups)){
          shinyWidgets::pickerInput(ns("task"), 
                                    label = "Task", 
                                    multiple = F, 
                                    choices = getTasks(config, withId = T, profile = profile), 
                                    selected = NULL,
                                    options = list(
                                      placeholder = "Select a task"
                                    ))
        }else{
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
        }
      })
      
      #go_readandmerge_wrapper
      output$go_readandmerge_wrapper <-renderUI({
        if(!is.null(input$task)) if(input$task != ""){
          actionButton(ns("go_readandmerge"),"Next")
        }else{
          NULL
        }
      })
      
      #readandmerge and go to step 2
      observeEvent(input$go_readandmerge,{
        shinyjs::disable("go_readandmerge")
        data_resources$task <- input$task
        data_resources$newdata <- readAndMergeAcceptedData(config = config, pool = pool, profile = profile, store = store, task_id = input$task)
        
        if(is.null(data_resources$newdata)){
          WARN("No new data accepted ready to persist for task '%s'", input$task)
          appendTab(inputId = "db-wizard-tabs",
                    session = parent.session,
                    select = TRUE,
                    tabPanel(
                      title="Completion",
                      value="db_end",
                      tagList(
                        br(),
                        p(sprintf("No new data accepted ready to persist for task '%s'", input$task)),
                        actionButton(ns("go_finish"),"Finish")
                      )
                    )
          )
          
        }else{
          INFO("Reading and merging accepted data for task '%s': %s rows", input$task, nrow(data_resources$newdata))
          data_resources$dbdata <- getDataTaskDBData(pool = pool, task_id = input$task)
          INFO("Getting DB data for task '%s': %s rows", input$task, nrow(data_resources$dbdata))
          data_resources$dbdatanew_duplicates <- combineData(data_resources$newdata, data_resources$dbdata, show_duplicates = TRUE)
          INFO("Getting combined data (with duplicates) for task '%s': %s rows", input$task, nrow(data_resources$dbdatanew_duplicates))
          data_resources$dbdatanew_noduplicates <- combineData(data_resources$newdata, data_resources$dbdata, show_duplicates = FALSE)
          INFO("Getting combined data (with no duplicates) for task '%s': %s rows", input$task, nrow(data_resources$dbdatanew_noduplicates))
          shinyjs::enable("go_readandmerge")
          
          appendTab(inputId = "db-wizard-tabs",
                    session = parent.session,
                    select = TRUE,
                    tabPanel(
                      title="2-Preview & storage", 
                      value="db_previewandpersist",
                      tagList(
                        br(),
                        p("Please verify if data displayed corresponds to the data to be stored."),
                        p("Data rows highlighted in ",tags$span("red", style = "color:#dd4b39;font-weight:bold;")," are duplicates series",
                          "and will be overwriten by new data highlighted in ",tags$span("green", style = "color:#008000;font-weight:bold;"),".", 
                          "If it is ok, please click 'Next' to proceed."),
                        DTOutput(ns("db_data_preview")),
                        actionButton(ns("goback_readandmerge"),"Previous"),
                        actionButton(ns("go_persist"),"Next")
                      )
                    )
          )
        }
      })
      
      #TAB 2 - MERGE DATA
      #TAB 2 MANAGER
      #----------------------
      output$db_data_preview <- DT::renderDT(server = TRUE, {
        if(nrow(data_resources$dbdatanew_duplicates)>0){
          DT::datatable(
            data_resources$dbdatanew_duplicates,
            rownames = FALSE,
            escape = FALSE,
            filter = list(position = 'top',clear =FALSE),
            options = list(
              dom = 'Bfrtip',
              scrollX=TRUE,
              pageLength=5
            )
          )%>% formatStyle(
            'duplicated',
            target = 'row',
            backgroundColor = styleEqual(c(TRUE, FALSE), c('#F3A5A8','#CEF3D6'))
          )
        }else{NULL}
      })
      
      #GO BACK TAB 1 FROM TAB 2
      observeEvent(input$goback_readandmerge,{
        removeTab(inputId = "db-wizard-tabs", 
                  session = parent.session,
                  target = "db_previewandpersist")
        updateTabsetPanel(inputId = "db-wizard-tabs", 
                          session = parent.session,
                          selected = "db_merge_data")
      })
      
      #persist data and go to step 3
      observeEvent(input$go_persist,{
        shinyjs::disable("go_persist")
        persistData(session = session, config = config, pool = pool, profile = profile, store = store)
        shinyjs::enable("go_persist")
        appendTab(inputId = "db-wizard-tabs",
                  session = parent.session,
                  select = TRUE,
                  tabPanel(
                    title="Completion",
                    value="db_end",
                    tagList(
                      br(),
                      p("Data has been successfully stored in the database!"),
                      actionButton(ns("go_finish"),"Finish")
                    )
                  )
        )
      })
      
      observeEvent(input$go_finish,{
        restart(TRUE)
        reloader <- reloader(id)
        removeTab(inputId = "db-wizard-tabs", session = parent.session, target = "db_merge_data")
        removeTab(inputId = "db-wizard-tabs", session = parent.session, target = "db_previewandpersist")
        removeTab(inputId = "db-wizard-tabs", session = parent.session, target = "db_services")
        removeTab(inputId = "db-wizard-tabs", session = parent.session, target = "db_end")
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}