function(id, parent.session, config, profile, components, reloader){
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
          newdata <- do.call("rbind", lapply(1:nrow(data_submissions), function(i){
            data_submission <- data_submissions[i,]
            item <- store$getWSItem(parentFolderID = data_submission$id, itemPath = paste0(data_submission$data_call_folder, ".csv"))
            filename <- store$downloadItem(item = item, wd = tempdir())
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
        out <- rbind(newdata,dbdata)
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
        storeInDataspace(config, pool, profile, store, task_id, newdata_file)
        unlink(newdata_file)
        
        #store db backup in dataspace
        if(!is.null(data_resources$dbdata)){
          dbdata_file <- paste0(datetime, "_db_old.csv")
          progress$set(value = 2, message = sprintf("Store DB data dump in dataspace: file '%s'", dbdata_file))
          readr::write_csv(data_resources$dbdata, dbdata_file)
          storeInDataspace(config, pool, profile, store, task_id, dbdata_file)
          unlink(dbdata_file)
        }
        
        #store db new version in workspace
        dbdatanew_file <- paste0(datetime, "_db_new.csv")
        progress$set(value = 3, message = sprintf("Store DB updated data in dataspace: file '%s'", dbdatanew_file))
        readr::write_csv(data_resources$dbdatanew_noduplicates, dbdatanew_file)
        storeInDataspace(config, pool, profile, store, task_id, dbdatanew_file)
        unlink(dbdatanew_file)
        
        #store new data in the database
        table_id <- getDataTaskTablename(task_id)
        progress$set(value = 4, message = sprintf("Write data in database table '%s'", table_id))
        DBI::dbWriteTable(conn = pool, name = table_id, value = data_resources$dbdatanew_noduplicates, overwrite = TRUE)
        
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
                              tabsetPanel(id = "rdb-wizard-tabs",
                                          type="pills",
                                          tabPanel(title="RDB data publisher",
                                                   value="rdb_home",
                                                   select = TRUE,
                                                   h2("Welcome to the WECAFC-FIRMS RDB data publisher"),
                                                   p("Within this module you you will be able to:"),
                                                   tags$ul(
                                                     tags$li("Merge accepted data submissions for defined data dasks"),
                                                     tags$li("Check merged data and store it into the WECAFC-FIRMS RDB"),
                                                     tags$li("Trigger geoflow for publication of RDB (meta)data services and update of WECAFC-FIRMS data map viewer")
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
        appendTab(inputId = "rdb-wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="1-Merge data", 
                    value ="rdb_merge_data",
                    tagList(
                      br(),
                      uiOutput(ns("task_wrapper")),
                      uiOutput(ns("go_readandmerge_wrapper"))
                    )
                  )
        )
        removeTab(inputId = "rdb-wizard-tabs", 
                  session = parent.session,
                  target = "rdb_home")
        updateTabsetPanel(inputId = "rdb-wizard-tabs", 
                          session = parent.session,
                          selected = "rdb_merge_data")
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
          appendTab(inputId = "rdb-wizard-tabs",
                    session = parent.session,
                    select = TRUE,
                    tabPanel(
                      title="Completion",
                      value="rdb_end",
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
          
          appendTab(inputId = "rdb-wizard-tabs",
                    session = parent.session,
                    select = TRUE,
                    tabPanel(
                      title="2-Preview & storage", 
                      value="rdb_previewandpersist",
                      tagList(
                        br(),
                        p("Please verify if data displayed corresponds to the data to be stored."),
                        p("Data rows highlighted in ",tags$span("red", style = "color:#dd4b39;font-weight:bold;")," are duplicates series",
                          "and will be overwriten by new data highlighted in ",tags$span("green", style = "color:#008000;font-weight:bold;"),".", 
                          "If it is ok, please click 'Next' to proceed."),
                        DTOutput(ns("rdb_data_preview")),
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
      output$rdb_data_preview <- DT::renderDT(server = FALSE, {
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
        removeTab(inputId = "rdb-wizard-tabs", 
                  session = parent.session,
                  target = "rdb_previewandpersist")
        updateTabsetPanel(inputId = "rdb-wizard-tabs", 
                          session = parent.session,
                          selected = "rdb_merge_data")
      })
      
      #persist data and go to step 3
      observeEvent(input$go_persist,{
        shinyjs::disable("go_persist")
        persistData(session = session, config = config, pool = pool, profile = profile, store = store)
        shinyjs::enable("go_persist")
        if(input$task == "task-I.2"){
          appendTab(inputId = "rdb-wizard-tabs",
                    session = parent.session,
                    select=TRUE,
                    tabPanel(
                      title="3-Data services", 
                      value="rdb_services",
                      tagList(
                        br(),
                        p("Data has been successfully stored in the database!"),
                        p("This last step will update relevant resources in the WECAFC-FIRMS SDI including metadata (ISO-19115 and ISO-19110) published
                          as ISO-19139 in the GeoNetwork catalogue, and data published in GeoServer; both required for the update of the WECAFC-FIRMS 
                          data map viewer."),
                        actionButton(ns("go_geoflow"),"Publish")
                      )
                    )
          )
        }else{
          WARN("No data service specification available for '%s'", input$task)
          appendTab(inputId = "rdb-wizard-tabs",
                    session = parent.session,
                    select = TRUE,
                    tabPanel(
                      title="Completion",
                      value="rdb_end",
                      tagList(
                        br(),
                        p("Data has been successfully stored in the database!"),
                        actionButton(ns("go_finish"),"Finish")
                      )
                    )
          )
        }
        
       
      })
      
      observeEvent(input$go_geoflow,{
        Sys.setenv(TASK_ID = data_resources$task)
        Sys.setenv(
          DB_HOST = config$dbi$host, 
          DB_PORT = config$dbi$port, 
          DB_USER = config$dbi$user, 
          DB_PASSWORD = config$dbi$password,
          DB_DBNAME = config$dbi$dbname
        )
        Sys.setenv(
          GEONETWORK_URL = components$GEONETWORK_CONFIG$url,
          GEONETWORK_VERSION = components$GEONETWORK_CONFIG$version,
          GEONETWORK_USER = components$GEONETWORK_CONFIG$user, 
          GEONETWORK_PASSWORD = components$GEONETWORK_CONFIG$pwd
        )
        Sys.setenv(
          GEOSERVER_URL = components$GEOSERVER_CONFIG$url,
          GEOSERVER_USER = components$GEOSERVER_CONFIG$user,
          GEOSERVER_PASSWORD = components$GEOSERVER_CONFIG$pwd
        )
        shinyjs::disable("go_geoflow")
        download.file("https://raw.githubusercontent.com/eblondel/dcf-shiny/main/modules/plugins/wecafc-firms/geoflow/wecafc-firms_geoflow.json", mode = "wb", 
                      destfile = file.path(tempdir(), "wecafc-firms_geoflow.json"), quiet = TRUE)
        geoflow::executeWorkflow(file.path(tempdir(), "wecafc-firms_geoflow.json"), dir = tempdir())
        shinyjs::enable("go_geoflow")
        
        Sys.unsetenv("TASK_ID")
        Sys.unsetenv("GEONETWORK_URL")
        Sys.unsetenv("GEONETWORK_VERSION")
        Sys.unsetenv("GEONETWORK_USER")
        Sys.unsetenv("GEONETWORK_PASSWORD")
        Sys.unsetenv("GEOSERVER_URL")
        Sys.unsetenv("GEOSERVER_USER")
        Sys.unsetenv("GEOSERVER_PASSWORD")
        
        appendTab(inputId = "rdb-wizard-tabs",
                  session = parent.session,
                  select = TRUE,
                  tabPanel(
                    title="Completion",
                    value="rdb_end",
                    tagList(
                      br(),
                      p(sprintf("Data successfully uploaded and services enabled for task '%s'", input$task)),
                      actionButton(ns("go_finish"),"Finish")
                    )
                  )
        )
        
      })
      
      observeEvent(input$go_finish,{
        restart(TRUE)
        removeTab(inputId = "rdb-wizard-tabs", session = parent.session, target = "rdb_merge_data")
        removeTab(inputId = "rdb-wizard-tabs", session = parent.session, target = "rdb_previewandpersist")
        removeTab(inputId = "rdb-wizard-tabs", session = parent.session, target = "rdb_services")
        removeTab(inputId = "rdb-wizard-tabs", session = parent.session, target = "rdb_end")
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}