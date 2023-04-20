data_validation_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      store <- components$STORAGEHUB
      
      ns <- session$ns
      
      restart<-reactiveVal(TRUE)
      keycolor_list<-setNames(rainbow(length(geometa::ISOKeywordType$values())),geometa::ISOKeywordType$values())
      
      #RESTART FUNCTION
      restartProcess<-function(initialize=F){
        dataCall<<-reactiveVal(FALSE)
        gbOut<<-reactiveVal(NULL)
        gbReportPath<<-reactiveVal(NULL)
        dcOut<<-reactiveVal(NULL)
        dcReportPath<<-reactiveVal(NULL)
        taskProperties<<-reactiveVal(NULL)
        loadedData<<-reactiveVal(NULL)
        file_info<<-reactiveValues(
          datapath = NULL,
          name = NULL
        )
        transformation<<-reactiveValues(
          data_reformat = FALSE,
          data_rename = FALSE
        )
        submission <<- reactiveValues(
          data_call_id = NULL,
          task_id = NULL,
          task_name =NULL,
          reporting_entity = NULL,
          notes = "-"
        )
        submitted<<-reactiveVal(NULL)
        if(!initialize){
          Sys.unsetenv("DATA_CALL_YEAR")
         # output$globalValidReport<<-renderUI({NULL})
          #output$callValidReport<<-renderUI({NULL})
          output$dataCallMessage<<-renderUI({NULL})
        }
        
        table_relation<<-reactiveVal(
          data.frame(
            type=character(0),
            description=character(0),
            link=character(0)
          ))
        
        table_keyword<<-reactiveVal(
          data.frame(
            type=character(0),
            description=character(0),
            link=character(0)
          ))
        
        table_process<<-reactiveVal(
          data.frame(
            id=character(0),
            name=character(0),
            description=character(0),
            link=character(0)
          ))
        keywords<<-reactiveVal(NULL)
        keywords_color<<-reactiveVal(NULL)
        
      }
      #Initialize reactive values
      #-----------------------------------------------------------------------------------
      restartProcess(initialize=T)
      
      #Initialize module content (Home page)
      #-----------------------------------------------------------------------------------
      #HOME
      observeEvent(restart(),{
        print("CLICKED ON FINISH")
        req(isTRUE(restart()))
        output$wizard<-renderUI({
          
          tags$div(class ="row",
           tags$div(class = "col-md-12",
             tagList(     
              tags$div(class = "connecting-line"),
              tabsetPanel(id = "wizard-tabs",
                      type="pills",
                      tabPanel(title="Home",
                               value="home",
                               h2("Welcome to the Data validation and Submission module"),
                               p("Within this module you you will be able to:"),
                               tags$ul(
                                 tags$li("Check the validity of your data against standards (CWP, FIRMS, etc)"),
                                 tags$li("Submit your data as part of data calls")
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
      
      #Restart module content (Home page) if 'Finish' button is clicked
      #-----------------------------------------------------------------------------------

      
      #GO BACK TAB 1 FROM TAB 2
      observeEvent(input$goData,{
        removeTab(inputId = "wizard-tabs", 
                  session = parent.session,
                  target = "preview")
        updateTabsetPanel(inputId = "wizard-tabs", 
                          session = parent.session,
                          selected = "select_data")
      })
      #GO BACK HOME FROM TAB 3
      observeEvent(input$close1,{
        restartProcess()
        restart<-restart(TRUE)
      })
      #GO BACK HOME FROM TAB 4
      observeEvent(input$close2,{
        restartProcess()
        restart<-restart(TRUE)
      })
      #GO BACK HOME FROM TAB 6
      observeEvent(input$close3,{
        restartProcess()
        restart<-restart(TRUE)
      })
      #GO BACK HOME FROM TAB 6 MODAL
      observeEvent(input$close4,{
        restartProcess()
        restart<-restart(TRUE)
        removeModal()
      })
      
      #Wizard panels routine
      #-----------------------------------------------------------------------------------
      #TAB 1 - SELECT YOUR DATA
      #TAB 1 MANAGER
      observeEvent(input$start,{
        restart<-restart(FALSE)
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="1-Select your data", 
                    value ="select_data",
                    tagList(
                      uiOutput(ns("task_wrapper")),
                      uiOutput(ns("dataCallMessage")),
                      uiOutput(ns("reporting_entity_wrapper")),
                      uiOutput(ns("format_wrapper")),
                      uiOutput(ns("file_wrapper")),
                      #Next
                      uiOutput(ns("goPreview_wrapper"))
                    )
                  )
        )
        removeTab(inputId = "wizard-tabs", 
                  session = parent.session,
                  target = "home")
        updateTabsetPanel(inputId = "wizard-tabs", 
                          session = parent.session,
                          selected = "select_data")
      })
      #TAB 1 TASK SELECTOR
      output$task_wrapper<-renderUI({
        selectizeInput(ns("task"),
                       label="Task",
                       multiple = F,
                       choices = getTasks(config,withId=TRUE),
                       selected=NULL,
                       options = list(
                         placeholder = "Please select a task",
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        )
      })
      #TAB 1 REPORTING ENTITY SELECTOR
      output$reporting_entity_wrapper <- renderUI({
        has_task <- FALSE
        if(!is.null(input$task))if(input$task!="") has_task <- TRUE
        if(has_task){
        if(!is.null(profile$reporting_entities)) {
          if(all(profile$reporting_entities != "")){
            if(config$dcf$reporting_entities$name %in% c("country", "flagstate")){
              selectizeInput(ns("reporting_entity"), label = "Reporting entity", selected = NULL, multiple = FALSE, 
               choices = {
                 ref_entity <- getReportingEntityCodes(config)
                 ref_entity <- ref_entity[ref_entity$code %in% getDBUserReportingEntities(profile = profile, pool = pool),]
                 entity_choices <- ref_entity$code
                 setNames(entity_choices, ref_entity$label)
               },options = list( 
                 render = I("{
                      item: function(item, escape) {
                        var icon_href = 'https://raw.githubusercontent.com/fdiwg/flags/main/'+item.value.toLowerCase()+'.gif';
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      },
                      option: function(item, escape) { 
                        var icon_href = 'https://raw.githubusercontent.com/fdiwg/flags/main/'+item.value.toLowerCase()+'.gif';
                        return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                      }
                    }"
                 ),
                 placeholder = "Please select a reporting entity",
                 onInitialize = I('function() { this.setValue(""); }')
                )
              )
            }else if(config$dcf$reporting_entities$name == "rfmo"){
              selectizeInput(ns("reporting_entity"), label = "Reporting entity", selected = NULL, multiple = FALSE, 
                             choices = {
                               ref_entity <- getReportingEntityCodes(config)
                               ref_entity <- ref_entity[ref_entity$code %in% getDBUserReportingEntities(profile = profile, pool = pool),]
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
                               ),
                               placeholder = "Please select a reporting entity",
                               onInitialize = I('function() { this.setValue(""); }')
                             )
              )
            }else{
              selectizeInput(ns("reporting_entity"), label = "Reporting entity", selected = NULL, multiple = FALSE,
                choices = {
                  ref_entity <- getReportingEntityCodes(config)
                  ref_entity <- ref_entity[ref_entity$code %in% getDBUserReportingEntities(profile = profile, pool = pool),]
                  entity_choices <- ref_entity$code
                  setNames(entity_choices, ref_entity$label)
                }, options = list(
                  placeholder = "Please select a reporting entity",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            }
          }else{
            tags$p("At least one reporting entity is required for validating your data. Please contact your system data manager!", style="color:red;font-weight:bold;")
          }
        }else{
          tags$span("")
        }
        }else{
          NULL
        }
      })
      #TAB 1 FORMAT SELECTOR
      output$format_wrapper<-renderUI({
        has_task <- FALSE
        if(!is.null(input$task))if(input$task!="") has_task <- TRUE
        has_reporting_entity <- TRUE
        if(!is.null(config$dcf$reporting_entities)){
          has_reporting_entity <- FALSE
          if(!is.null(input$reporting_entity))if(input$reporting_entity!="") has_reporting_entity <- TRUE
        }
        if(has_task && has_reporting_entity){
          selectizeInput(ns("format"),
                         label="Data format",
                         multiple = F,
                         choices = c("Simplified"="simplified",
                                     "Generic"="generic"),
                         selected=NULL,
                         options = list(
                           placeholder = "Please select a format",
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        }else{
          NULL
        }
      })
      #TAB 1 FILE SELECTOR
      output$file_wrapper<-renderUI({
        if(!is.null(input$format))if(input$format!=""){
          fileInput(ns("file"), label = "File to test",multiple = FALSE,accept = c(".xlsx",".xls",".csv",".zip"),buttonLabel = "Choose file")
        }else{
          NULL
        }
      })
      #TAB 1 BUTTONS
      output$goPreview_wrapper<-renderUI({
        if(!is.null(input$file)){
          actionButton(ns("goPreview"),"Next")
        }else{
          NULL
        }
      })
      
      #CHECK IF DATACALL IS OPEN FOR SELECTED TASK
      observeEvent(input$task,{
        if(!is.null(input$task))if(input$task!=""){
        #Check Data call open
        taskProfile<-getTaskProperties(config,id=input$task)
        taskProperties<-taskProperties(taskProfile)
        datacall<-getDataCalls(pool,status="OPENED",tasks=input$task,period="IN")
          if(nrow(datacall)==1){
            Sys.setenv(DATA_CALL_YEAR = as.integer(format(datacall$creation_date, "%Y")))
            dataCall<-dataCall(TRUE)
            submission$data_call_id = datacall$id_data_call
            submission$task_id = datacall$task_id
            submission$task_name =taskProperties()$name
            output$dataCallMessage<-renderUI({tags$span(shiny::icon(c('check-circle')), "A data call is currently open for this task", style="color:green;")})
          }
          
          if(nrow(datacall)==0){
            dataCall<-dataCall(FALSE)
            Sys.unsetenv("DATA_CALL_YEAR")
            output$dataCallMessage<-renderUI({tags$span(shiny::icon(c('exclamation-triangle')), "No data call is currently open for this task, your dataset can only be tested for validation but not sent to manager", style="color:orange;")})
          }
        }
        })
      
      #TAB 2 - PREVIEW
      output$dataView<-DT::renderDT(server = FALSE, {
        req(input$file)
        if(!is.null(input$file)){
          if(any(endsWith(input$file$datapath,c("xls","xlsx")))){
            data<-read_excel(input$file$datapath,col_types = "text")
            file_info$datapath<-input$file$datapath
            file_info$name<-input$file$name
          }else if(any(endsWith(input$file$datapath,"csv"))){
            data<-readr::read_csv(input$file$datapath,col_types = readr::cols(.default = "c"))
            file_info$datapath<-input$file$datapath
            file_info$name<-input$file$name
            print(file_info$name)
          }else if(any(endsWith(input$file$datapath,"zip"))){
            files<-zip_list(input$file$datapath)
            unzip(input$file$datapath,files=c(files$filename[1]),exdir = dirname(input$file$datapath))
            target_file<-file.path(dirname(input$file$datapath),files$filename[1])
            if(any(endsWith(target_file,c("xls","xlsx")))){
              data<-read_excel(target_file,col_types = "text")
            }else if(any(endsWith(target_file,"csv"))){
              data<-readr::read_csv(target_file,col_types = readr::cols(.default = "c"))
            }else{
              stop()
            }
            file_info$datapath<-target_file
            file_info$name<-basename(target_file)
            print(file_info$name)
          }else{
            stop()
          }
            
          loadedData<-loadedData(data)
          DT::datatable(
            data,
            escape = FALSE,
            filter = list(position = 'top',clear =FALSE),
            options = list(
              dom = 'Bfrtip',
              scrollX=TRUE,
              pageLength=5
            )
          )
        }else{NULL}
      })
      #TAB 2 MANAGER
      observeEvent(input$goPreview,{
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="2-Preview", 
                    value="preview",
                           tagList(
                             p("Please verify if data displayed correspond to the data to send. If it is, please click 'Next' to submit this file, otherwise click 'Previous' to select a new file."),
                             DTOutput(ns("dataView")),
                             #Previous
                             actionButton(ns("goData"),"Previous"),
                             #Next
                             actionButton(ns("goGlobValid"),"Next")
                           )
                  )
        )
      })
      
      #TAB 3 - RESPECT WITH STANDARD FORMAT
      #TAB 3 MANAGER
      observeEvent(input$goGlobValid,{
        taskRules <- taskProperties()$dsd_ref_url
        
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(title="3-Conformity with standards", 
                           value="standard_validation",
                           tagList(
                             uiOutput(ns("globalValidReport"))
                           )
                  )
        )
        
        hostess <- Hostess$new()
        hostess$set_loader(
          hostess_loader(
            preset = "circle", 
            text_color = "white",
            class = "label-center",
            style="margin:0 auto;",
            center_page = TRUE
          ))
        
        Sys.sleep(1)
        
        waiting_screen<-tagList(
          h3("Validation in progress"),
          #spin_flower(),
          hostess$get_loader(),
          h4("Please wait, validity of your dataset is currently being checked ...")
        )
        
        waiter_show(html = waiting_screen, color = "#14141480")
        data<-loadedData()
        INFO("Read Task '%s' column definitions", taskProperties()$name)
        task_def <- readTaskColumnDefinitions(file = taskRules, format = input$format, config = config, reporting_entity = input$reporting_entity)
        INFO("Validate data")
        out<-validateData(file=data, task_def = task_def, config = config,hostess=hostess)
        INFO("Successful data validation")
        if(out$valid){
          submission$reporting_entity <- input$reporting_entity
          data<-standardizeNames(file=data,task_def = task_def)
          if(!identical(names(data),names(loadedData()))){
            transformation$data_rename<-TRUE
          }
          hostess$set(95)
          print(head(as.data.frame(data),2))
          if(input$format=="simplified"){
            generic_task_def <- readTaskColumnDefinitions(file = taskRules, format = "generic", config = config, reporting_entity = input$reporting_entity)
            data<-simplifiedToGeneric(file=data, taskRules)
            transformation$data_reformat<-TRUE
            print(head(as.data.frame(data),2))
          }
          loadedData<-loadedData(data)
        }
        hostess$set(99)
        gbOut<-gbOut(out)
        print(names(loadedData()))

      })
      #TAB 3 REPORT ROUTINE
      output$globalValidReport<-renderUI({
        req(!is.null(gbOut()))
        out<-gbOut()
        
        #PDF Report
        info<-list(task_id=input$task,
                   date=Sys.Date(),
                   task_name=taskProperties()$name,
                   file=file_info$name,
                   format=input$format,
                   flagstate=input$reporting_entity)
        
        report_path<-file.path(tempdir(), sprintf("datacall-%s_task-%s_for_%s_report_standard_conformity.pdf",submission$data_call_id,input$task,input$reporting_entity))
        gbReportPath<-gbReportPath(report_path)
        print(report_path)
        print(out$errors)
        rmarkdown::render("assets/templates/report_standard_conformity_template.Rmd", output_file = report_path ,output_format = "pdf_document",output_options = list(keep_tex = TRUE), params = list(out,info,config))
        
        #HTML Report
        #Table with summary of rules
        output$gbSummary<-DT::renderDT(server = FALSE, {
          if(nrow(out$errors)!=0){
          x<-unique(subset(out$errors,select="rule"))
          x$status<-"FAILED"
          test<-merge(out$tests,x,by.x="code",by.y="rule",all.x=T,all.y=F)
          test[is.na(test)]<-"PASSED"
          test[startsWith(test$code,"I"),]$status<-"WARNING" 
          }else{
            test<-out$test
            test$status<-"PASSED"
          }
          test<-subset(test,select=-c(code))
          test$icon<-ifelse(test$status=="PASSED",paste0(tags$span(shiny::icon("check-circle"), title = "Passed", style = "color:green;"), collapse=""),
                            ifelse(test$status=="WARNING",paste0(tags$span(shiny::icon("exclamation-triangle"), title = "Warning", style = "color:orange;"), collapse=""),
                                   paste0(tags$span(shiny::icon("times-circle"), title = "Failed", style = "color:red;"), collapse="")))
          
          DT::datatable(
            test, 
            rownames = NULL,
            escape = FALSE,
            colnames = c('','',''), 
            options = list(dom = 't',
                           ordering=F,
                           columnDefs = list(list(className = 'dt-center', targets = 1:2))))%>%
            formatStyle("status",target = 'row',backgroundColor = styleEqual(c("PASSED","WARNING","FAILED"), c("#d3f4cc","#f5dfc0","#ffbfc1")))%>%
            formatStyle("status", fontWeight = 'bold',color= styleEqual(c("PASSED","WARNING","FAILED"), c("green","orange","red")))
          
        })
        
        #Table with details of errors
        output$gbErrors<-DT::renderDT(server = FALSE, {
          if(nrow(out$errors)>0){
            DT::datatable(
              out$errors,
              colnames = c("Type","Rule","Row","Column","Category","Message"), 
              extensions = c("Buttons"),
              escape = FALSE,
              filter = list(position = 'top',clear =FALSE),
              options = list(
                dom = 'Bfrtip',
                scrollX=TRUE,
                pageLength=5,
                buttons = list(
                  list(extend = 'csv', filename =  "errors_summary", title = NULL, header = TRUE)
                ),
                exportOptions = list(
                  modifiers = list(page = "all",selected=TRUE)
                )
              )
            )%>%
              formatStyle("type",target = 'row',backgroundColor = styleEqual(c("INFO","WARNING","ERROR"), c("#fcfdd0","#FDEBD0","#F2D7D5")))
          }else{NULL}
        })
        
        #Status message
        content<-tagList(
          if(out$valid){
            tags$div(shiny::icon(c('check-circle')), "Data is valid", style="margin-top:5px;color:green;font-size: 200%")
          }else{
            tags$div(shiny::icon(c('times-circle')), "Data is invalid", style="margin-top:5px;color:red;font-size: 200%")
          },
          br(),
          if(out$valid){
            p("Congratulations! Your data passed the validation step. You can see below the data analysis details and click 'next'")
          }else{
            p("Oops somethink seems to be incorrect in your data. Please see below the data analysis details and click 'finish' to return to home page and try again.")
          },
          br(),
          #Data informations
          fluidRow(
            column(3,style = "border: 1px solid black;",
                   p(strong("Task ID: "),input$task),
                   p(strong("Date of Report : "),Sys.Date())
            ),
            column(6,style = "border: 1px solid black;",
                   p(strong("Task Name: "),taskProperties()$name),
                   p(strong("File : "),file_info$name)
            ),
            column(3,style = "border: 1px solid black;",
                   p(strong("Format : "),input$format),
                   p(strong("Approved for Upload : "),span(ifelse(out$valid,"Yes","No"),style = ifelse(out$valid,"color:green","color:red")))
            )
          ),
          br(),
          fluidRow(
            column(6,
                   h3("VALIDITY SUMMARY"),
                   fluidRow(column(8,offset=2,DTOutput(ns("gbSummary"))))
            ),
            column(6,
                   h3("DETAILS"),
                   if(nrow(out$errors)>0){
                     DTOutput(ns("gbErrors"))
                   }else{
                     tags$span("No error detected",style = "color:green")
                   }
            )
          ),
          br(),
          tagList(
              if(transformation$data_rename|transformation$data_reformat){h3("DATA TRANSFORMATION")},
              if(transformation$data_rename){p("Please notice that some columns of the data file used a no official name and have been automatically renamed")},
              if(transformation$data_reformat){p("Please notice that the data file is in simplified format and have been automatically converted to generic format")}
          ),
          downloadButton(ns("gbreport"),"Download Report",icon=icon("download")),
          if(out$valid){
            #Next
            actionButton(ns("goSpecValid"),"Next")
          }else{
            #Close
            actionButton(ns("close1"),"Finish")
          }
        )
        waiter_hide()
        return(content)      
      })
      
      #Download Pdf 
      output$gbreport <- downloadHandler(
        filename = function(){ 
          gsub("pdf","zip",basename(gbReportPath())) },
        content = function(file){
          list_files<-c()
          tmpdir <- tempdir()
          current_path<-getwd()
          setwd(tempdir())
          list_files<-c(list_files,basename(gbReportPath()))
          out<-gbOut()
          if(nrow(out$errors)>0){
            file_path<-gsub(".pdf","_errors_detail.csv",basename(gbReportPath()))
            write.csv(out$errors,file_path,row.names = F)
            list_files<-c(list_files,file_path)
          }
          zip(zipfile=file,files=list_files)
          setwd(current_path)
          },
          contentType = "application/zip")

      #TAB 4 - CONSISTENCY WITH DATA CALL
      #TAB 4 MANAGER
      #TODO call validateCallRules once we added the tab
      
      observeEvent(input$goSpecValid,{
        
        hostess <- Hostess$new()
        hostess$set_loader(
          hostess_loader(
            preset = "circle", 
            text_color = "white",
            class = "label-center",
            style="margin:0 auto;",
            center_page = TRUE
          ))
        
        Sys.sleep(1)
        
        waiting_screen<-tagList(
          h3("Check consistency with the ongoing data call"),
          #spin_flower(),
          hostess$get_loader(),
          h4("Please wait, your dataset is currently checked vs. the ongoing data call ...")
        )
        
        waiter_show(html = waiting_screen, color = "#14141480")

        taskSupplRules<-taskProperties()$data_call_limited_on
        taskSupplRules$reporting_entity<-input$reporting_entity
        data<-loadedData()

        out<-validateCallRules(file = data,rules = taskSupplRules,hostess=hostess)
        dcOut<-dcOut(out)
        hostess$set(95)
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(title="4-Consistency with data Call", 
                           value="specific_validation",
                           tagList(
                             uiOutput(ns("callValidReport"))
                           )
                  )
        )

      })
      
      #TAB 4 REPORT ROUTINE
      output$callValidReport<-renderUI({
        shiny::req(!is.null(dcOut()))
        out<-dcOut()
        report_path<-file.path(tempdir(), sprintf("datacall-%s_task-%s_for_%s_report_datacall_consistency.pdf",submission$data_call_id,input$task,input$reporting_entity))
        dcReportPath<-dcReportPath(report_path)
        print(report_path)
        rmarkdown::render("assets/templates/report_datacall_consistency_template.Rmd", output_file = report_path ,output_format = "pdf_document",output_options = list(keep_tex = TRUE), params = list(out,submission,config))
        
        #HTML Report
        
        output$dcSummary<-DT::renderDT(server = FALSE, {
          test<-out$tests
          
          DT::datatable(
            test, 
            rownames = NULL,
            escape = FALSE,
            colnames = c('','',''), 
            options = list(dom = 't',
                           ordering=F,
                           columnDefs = list(list(className = 'dt-center', targets = 1:2))))%>%
            formatStyle("status",target = 'row',backgroundColor = styleEqual(c("PASSED","WARNING","FAILED","NOT TESTED"), c("#d3f4cc","#f5dfc0","#ffbfc1","#f0f0f0")))%>%
            formatStyle("status", fontWeight = 'bold',color= styleEqual(c("PASSED","WARNING","FAILED","NOT TESTED"), c("green","orange","red","grey")))
          
        })
        
        output$dcErrors<-DT::renderDT(server = FALSE, {
          if(nrow(out$errors)>0){
            DT::datatable(
              out$errors,
              colnames = c("Type","Rule","Row","Column","Category","Message"), 
              extensions = c("Buttons"),
              escape = FALSE,
              filter = list(position = 'top',clear =FALSE),
              options = list(
                dom = 'Bfrtip',
                scrollX=TRUE,
                pageLength=5,
                buttons = list(
                  list(extend = 'csv', filename =  "errors_summary", title = NULL, header = TRUE)
                ),
                exportOptions = list(
                  modifiers = list(page = "all",selected=TRUE)
                )
              )
            )%>%
              formatStyle("type",target = 'row',backgroundColor = styleEqual(c("INFO","WARNING","ERROR"), c("#fcfdd0","#FDEBD0","#F2D7D5")))
          }else{NULL}
        })
        
        
        content<-tagList(
          if(out$valid){
            tags$div(shiny::icon(c('check-circle')), "Data is ready to be submitted", style="margin-top:5px;color:green;font-size: 200%")
          }else{
            tags$div(shiny::icon(c('times-circle')), "Data is not consistent with the current data call", style="margin-top:5px;color:red;font-size: 200%")
          },
          br(),
          if(out$valid){
            p("Congratulations! Your data is consistent with the current data call. You can see below the data analysis details and click 'next'")
          }else{
            p("Oops somethink seems to be incorect in your data. Please see below the data analysis details and click 'finish' to return to home page and try again.")
          },
          fluidRow(
            column(6,
                   h3("VALIDITY SUMMARY"),
                   fluidRow(column(8,offset=2,DTOutput(ns("dcSummary"))))
            ),
            column(6,
                   h3("DETAILS"),
                   if(nrow(out$errors)>0){
                     fluidRow(column(8,offset=2,DTOutput(ns("dcErrors"))))
                   }else{
                     tags$span("No error detected",style = "color:green")
                   }
            )
          ),
          br(),
          downloadButton(ns("dcreport"),"Download Report",icon=icon("download")),
          if(out$valid){
            #Next
            actionButton(ns("goMetadata"),"Next")
          }else{
            #Close
            actionButton(ns("close2"),"Finish")
          }
        )
        waiter_hide()
        return(content)
      })
      
      #Download Pdf 
      output$dcreport <- downloadHandler(
        filename = function(){ 
          gsub("pdf","zip",basename(dcReportPath())) },
        content = function(file){
          list_files<-c()
          tmpdir <- tempdir()
          current_path<-getwd()
          setwd(tempdir())
          list_files<-c(list_files,basename(dcReportPath()))
          out<-dcOut()
          if(nrow(out$errors)>0){
            file_path<-gsub(".pdf","_errors_detail.csv",basename(dcReportPath()))
            write.csv(out$errors,file_path,row.names = F)
            list_files<-c(list_files,file_path)
          }
          zip(zipfile=file,files=list_files)
          setwd(current_path)
        },
        contentType = "application/zip")
      
      #TAB 5 - SEND DATA
      #TAB 5 MANAGER
      observeEvent(input$goMetadata,{
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("5-Metadata", 
                           tabBox(id = "metadata",title=NULL,height="600px",width = "100%",
                                  tabPanel(title=tagList(icon("file-pen"),"Identification"),
                                           value="tab_desc",
                                           box(title="Informations",collapsible = T,
                                               fluidRow(
                                                 column(3,textInput(ns("file_id"),"Identifier", value = tolower(gsub(" |-","_",unlist(strsplit(file_info$name,".",fixed=T))[1])), width = NULL, placeholder = "Add a identifier")),
                                                 column(3,textInput(ns("file_title"),"Title", value = "", width = NULL, placeholder = "Add a title")),
                                                 column(6,shiny::textAreaInput(ns("file_description"), value = "", label = "Abstract", placeholder = "Add a abstract"))
                                               )
                                             ),
                                           fluidRow(),
                                           box(title="Keywords",collapsible = T,
                                               fluidRow(
                                                  column(3,
                                                    selectizeInput(ns("keyword_type"),
                                                                  label="Type",
                                                                  multiple = F,
                                                                  choices = geometa::ISOKeywordType$values(),
                                                                  selected="theme"
                                                    )
                                                  ),
                                                  column(3,textInput(ns("keyword_description"),"Keyword",value = "", width = NULL, placeholder = "Add keyword")),
                                                  column(3,textInput(ns("keyword_link"),"Link",value = "", width = NULL, placeholder = "http://...")),
                                                  column(3,
                                                    shinyWidgets::circleButton(ns("add_keyword"),title="Add keyword",size="sm",label="",icon=icon("plus"),class = "btn-success"),
                                                    shinyWidgets::circleButton(ns("clear_keyword"),title="Clear keywords",size="sm",label="",icon=icon("trash"),class = "btn-warning")
                                                  )
                                                ),
                                                fluidRow(
                                                  div(uiOutput(ns("keyword_list")))
                                                ),
                                               uiOutput(ns("keyword_table_wrapper"))
                                           )
                                  ),
                                  tabPanel(title=tagList(icon("link"),"Relations"),
                                           value="tab_relation",
                                           box(title="Relations",collapsible = T,
                                               fluidRow(
                                                 column(3, selectizeInput(ns("relation_type"),
                                                                          label="Type",
                                                                          multiple = F,
                                                                          choices = c("website","metadata","data"),
                                                                          selected="website"
                                                 )),
                                                 column(3,textInput(ns("relation_description"),"Description",value = "", width = NULL, placeholder = "Relation description")),
                                                 column(3,textInput(ns("relation_link"),"Link",value = "", width = NULL, placeholder = "http://...")),
                                                 column(3,
                                                        shinyWidgets::circleButton(ns("add_relation"),title="Add relation",size="sm",label="",icon=icon("plus"),class = "btn-success"),
                                                        shinyWidgets::circleButton(ns("clear_relation"),title="Clear relation",size="sm",label="",icon=icon("trash"),class = "btn-warning"))
                                               ),
                                               uiOutput(ns("relation_table_wrapper"))
                                           )
                                  ),
                                  tabPanel(title=tagList(icon("list-check"),"Provenance"),
                                           value="tab_process",
                                           div(
                                             textInput(ns("process_statement"),"Statement", value = "Data processing", width = NULL, placeholder = "Declare statement"),
                                             box(title="Processes",collapsible = T,
                                                 fluidRow(
                                                   column(3,textInput(ns("process_name"),"Title",value = "", width = NULL, placeholder = "Process title")),
                                                   column(3,textInput(ns("process_description"),"Description",value = "", width = NULL, placeholder = "Process description")),
                                                   column(3,textInput(ns("process_link"),"Link",value = "", width = NULL, placeholder = "http://...")),
                                                   column(3,
                                                    shinyWidgets::circleButton(ns("add_process"),title="Add process",size="sm",label="",icon=icon("plus"),class = "btn-success"),
                                                    shinyWidgets::circleButton(ns("clear_process"),title="Clear process",size="sm",label="",icon=icon("trash"),class = "btn-warning"))
                                                  ),
                                                  uiOutput(ns("process_table_wrapper"))
                                             )
                                           )
                                  )
                           ),
                           actionButton(ns("goSend"),"Next")

                  )
        )
      })
      
      observeEvent(input$add_relation, {
        req(input$relation_type!="")
        req(input$relation_description!="")
        req(input$relation_link!="")
        
        new_relation<-data.frame(
          type=input$relation_type,
          description=input$relation_description,
          link=input$relation_link
        )
        table_relation<-table_relation(rbind(table_relation(),new_relation))
        updateTextInput(session, "relation_description",value = "")
        updateTextInput(session, "relation_link",value = "")
      })
      
      observeEvent(input$clear_relation, {
        table_relation<-table_relation(
          data.frame(
          type=character(0),
          description=character(0),
          link=character(0)
        ))
        updateTextInput(session, "relation_description",value = "")
        updateTextInput(session, "relation_link",value = "")
      })
      
      output$relation_table<-DT::renderDT(server = FALSE, {
        DT::datatable(
          table_relation(), 
          escape = FALSE,
          options = list(dom = 't',
                         ordering=F)
        )
      })
      
      output$relation_table_wrapper<-renderUI({
        if(nrow(table_relation())>0){
          DTOutput(ns("relation_table"))
        }else{NULL}
      })
      
      observeEvent(input$add_keyword, {
        req(input$keyword_type!="")
        req(input$keyword_description!="")
        keyword_label<-paste0(input$keyword_type,":",input$keyword_description)
        keywords<-keywords(c(keywords(),if(startsWith(input$keyword_link,"http")){sprintf("<a href='%s' target='_blank'>%s</a>",input$keyword_link,keyword_label)}else{keyword_label}))
        print(keywords())
        keywords_color<-keywords_color(c(keywords_color(),keycolor_list[input$keyword_type][[1]]))
        keywords_url<-c(keywords_url(),input$keyword_link)
        
        new_keyword<-data.frame(
          type=input$keyword_type,
          description=input$keyword_description,
          link=input$keyword_link
        )
        table_keyword<-table_keyword(rbind(table_keyword(),new_keyword))
        updateTextInput(session, "keyword_description",value = "")
        updateTextInput(session, "keyword_link",value = "")
      })
      
      observeEvent(input$clear_keyword, {
        
        keywords<-keywords(NULL)
        keywords_color<-keywords_color(NULL)
        
        table_keyword<-table_keyword(
          data.frame(
            type=character(0),
            description=character(0),
            link=character(0)
          ))
        updateTextInput(session, "keyword_description",value = "")
        updateTextInput(session, "keyword_link",value = "")
      })
      
      output$keyword_table<-DT::renderDT(server = FALSE, {
        DT::datatable(
          table_keyword(), 
          escape = FALSE,
          options = list(dom = 't',
                         ordering=F)
        )
      })
      
      output$keyword_table_wrapper<-renderUI({
        if(nrow(table_keyword())>0){
          DTOutput(ns("keyword_table"))
        }else{NULL}
      })
      
      output$keyword_list<-renderUI({
        req(!is.null(keywords))
        tagList(
          HTML(paste0("<span> Keywords : </span>",paste0(sprintf("<span class='badge' style='background-color:%s'>%s</span>",keywords_color(),keywords()),collapse=" ")))
          )
      })
      
      observeEvent(input$add_process, {
        req(input$process_name!="")
        req(input$process_description!="")
        
        new_process<-data.frame(
          id=paste0("Process ",nrow(table_process())+1),
          title=input$process_name,
          description=input$process_description,
          link=input$process_link
        )
        table_process<-table_process(rbind(table_process(),new_process))
        print(table_process())
        updateTextInput(session, "process_name",value = "")
        updateTextInput(session, "process_description",value = "")
        updateTextInput(session, "process_link",value = "")
      })
      
      observeEvent(input$clear_process, {
        table_process<-table_process(
          data.frame(
            id=character(0),
            title=character(0),
            description=character(0),
            link=character(0)
          ))
        updateTextInput(session, "process_name",value = "")
        updateTextInput(session, "process_description",value = "")
        updateTextInput(session, "process_link",value = "")
      })
      
      output$process_table<-DT::renderDT(server = FALSE, {
        DT::datatable(
          table_process(), 
          escape = FALSE,
          options = list(dom = 't',
                         ordering=F)
        )
      })
      
      output$process_table_wrapper<-renderUI({
        if(nrow(table_process())>0){
          DTOutput(ns("process_table"))
        }else{NULL}
      })

      #TAB 6
      observeEvent(input$goSend,{
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("6-Send Data",
                     tagList(
                       p("You are going to send your data to the manager."),
                       p("Validity reports (conformity with standards, consistency with data call) will be attached to the submission"),
                       p("You may also add notes to the submission here below. Once ready, click on 'Send' to proceed with the submission"),
                       shiny::textAreaInput(ns("message"), value = submission$notes, label = "Submission notes", placeholder = "Add submission notes"),br(),
                       actionButton(ns("send"),"Send")
                     )
                  )
        )
      })
      
      #TAB 7 - THANK YOU
      #TAB 7 MANAGER
      
      submitData<- function(new=TRUE,session,dc_folder,submission,pool,profile,store,config,input){
        
        hostess <- Hostess$new()
        hostess$set_loader(
          hostess_loader(
            preset = "circle", 
            text_color = "white",
            class = "label-center",
            style="margin:0 auto;",
            center_page = TRUE
          ))
        
        # to not have the screen flash bright white
        Sys.sleep(1)
        
        waiting_screen<-tagList(
            h3(id="wait_title","Data submission"),
            hostess$get_loader(),
            div(id="wait_detail","Please wait, your dataset are currently being submitted...")
          )
        
        waiter_show(html = waiting_screen, color = "#14141480")
        
        uploadedOriginalDataId<-NULL
        uploadedDataId<-NULL
        uploadedMetadataId<-NULL
        uploadedReportStandardConformityId<-NULL
        uploadedReportDatacallConsistencyId<-NULL
        
        #progress <- shiny::Progress$new(session, min = 0, max = 100)
        #on.exit(progress$close())
        
        # progress$set(
        #   message = "Start data submission", 
        #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
        #   value = 0
        # )
        
        shared <- FALSE
        
        dc_title <- sprintf("Data submission for data call '%s' - task %s - reporting entity '%s'", 
                            submission$data_call_id, submission$task_id, submission$reporting_entity)
        dc_description <- sprintf("Data submission created by %s for data call '%s' - task %s - reporting entity '%s'. Notes from data submitter: %s", 
                                  profile$name, submission$data_call_id, submission$task_id, submission$reporting_entity, submission$notes)
        
        if(new){
          INFO("No submission yet for data call '%s' (task %s)", submission$data_call_id, submission$task_id)
          
          #create data call submission folder
          # progress$set(
          #   message = "Create data submission folder", 
          #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
          #   value = 10
          # )
          
          shinyjs::html(id="wait_detail",html=sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity))
          hostess$set(10)
          
          dc_folder_id <- store$createFolder(
            folderPath = config$dcf$user_workspace, 
            name = dc_folder,
            description = dc_description
          )
        }else{
          INFO("A submission is already created for data call '%s' (task %s)", submission$data_call_id, submission$task_id)
          #delete previous original file
          submit_folder_id<-getSubmissions(config,pool,profile, store, user_only = T,data_calls_id = submission$data_call_id,full_entities=F,status=NULL,reporting_entities=submission$reporting_entity)$id
          old_file_list<-store$listWSItems(parentFolderID = submit_folder_id)
          to_delete<-subset(old_file_list,!startsWith(title,dc_folder))$title
          store$deleteItem(itemPath = file.path(config$dcf$user_workspace, dc_folder,to_delete))
        }
        
        #upload data to data call submission folder
        # progress$set(
        #   message = "Upload data files", 
        #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
        #   value = 25
        # )
        shinyjs::html(id="wait_detail",html=sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity))
        hostess$set(25)
        
        #original file
        new_filename<-file.path(dirname(file_info$datapath),file_info$name)
        file.rename(file_info$datapath,new_filename)
        uploadedOriginalDataId <- store$uploadFile(folderPath = file.path(config$dcf$user_workspace, dc_folder), file = new_filename, description ="Original dataset")
        
        #file for submission
        if(!is.null(uploadedOriginalDataId)){
          INFO("Successful upload for source file '%s'", file_info$datapath)
          data_filename <- file.path(getwd(), paste0(dc_folder, ".csv"))
          readr::write_csv(loadedData(), data_filename)
          uploadedDataId <- store$uploadFile(folderPath = file.path(config$dcf$user_workspace, dc_folder), file = data_filename, description ="Formated dataset")
          unlink(data_filename)
        }
        
        if(!is.null(uploadedDataId)){
          INFO("Successful upload for data submission file '%s'", data_filename)
          
          # progress$set(
          #   message = "Upload metadata file", 
          #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
          #   value = 50
          # )
          shinyjs::html(id="wait_detail",html=sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity))
          hostess$set(50)
          
          #metadata
          dc_entry_filename <- file.path(getwd(), paste0(dc_folder, ".xml"))
          dc_entry <- atom4R::DCEntry$new()
          dc_entry$addDCDateSubmitted(dc_entry$updated)
          dc_entry$addDCTitle(dc_title)
          dc_entry$addDCAbstract(dc_description)
          dc_entry$addDCCreator(profile$name)
          dc_entry$addDCConformsTo(paste(config$dcf$context, "DCF"))
          dc_entry$addDCConformsTo("FIRMS data exchange format specifications")
          dc_entry$addDCConformsTo("CWP Standards for fishery purpose")
          dc_entry$addDCCoverage(paste0(config$dcf$reporting_entities$name,":",submission$reporting_entity))
          dc_entry$addDCSource(store$getPublicFileLink(path = file.path(config$dcf$user_workspace, dc_folder, basename(data_filename))))
          dc_entry$addDCFormat("text/csv")
          
          start <- min(as.Date(loadedData()$time_start))
          end <- max(as.Date(loadedData()$time_end))
          dc_entry$addDCTemporal(paste(start,end,sep="/"))
          
          dc_entry$save(dc_entry_filename)
          uploadedMetadataId <- store$uploadFile(folderPath = file.path(config$dcf$user_workspace, dc_folder), file = dc_entry_filename, description = "Metadata")
          
          if(!is.null(uploadedMetadataId) ){
            INFO("Successful upload for metadata file '%s'", dc_entry_filename)
          }
          unlink(dc_entry_filename)
        }
        
        if(!is.null(uploadedDataId) && !is.null(uploadedMetadataId)){
          if(!is.null(gbReportPath())){
            report_standard_conformity_filename<-gbReportPath()
            uploadedReportStandardConformityId <- store$uploadFile(folderPath = file.path(config$dcf$user_workspace, dc_folder), file = report_standard_conformity_filename, description ="Standard conformity report")
            if(!is.null(uploadedReportStandardConformityId)){
              INFO("Successful upload for standard conformity report file '%s'", report_standard_conformity_filename)
            }
            unlink(dc_entry_filename)
          }
          if(!is.null(dcReportPath())){
            report_datacall_consistency_filename<-dcReportPath()
            uploadedReportDatacallConsistencyId <- store$uploadFile(folderPath = file.path(config$dcf$user_workspace, dc_folder), file = report_datacall_consistency_filename, description ="Datacall consistancy report")
            if(!is.null(uploadedReportDatacallConsistencyId)){
              INFO("Successful upload for datacall consistancy report file '%s'", report_datacall_consistency_filename)
            }
            unlink(dc_entry_filename)
          }
        }
        
        #sharing
        if(!is.null(uploadedDataId) && !is.null(uploadedMetadataId) && !is.null(uploadedReportStandardConformityId) && !is.null(uploadedReportDatacallConsistencyId)){
          # progress$set(
          #   message = sprintf("Share data submission folder with %s", config$dcf$roles$manager), 
          #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
          #   value = 80
          # )
          
          shinyjs::html(id="wait_detail",html=sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity))
          hostess$set(80)
          
          #shared <- store$shareItem(itemPath = file.path(config$dcf$user_workspace, dc_folder), defaultAccessType = "WRITE_ALL", users = "emmanuel.blondel")
          shared=TRUE
        }
        
        #notification
        if(shared){
          #send notification
          # progress$set(
          #   message = sprintf("Notify %s", config$dcf$roles$manager), 
          #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
          #   value = 90
          # )
          
          shinyjs::html(id="wait_detail",html=sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity))
          hostess$set(90)
          
          if(new){
            body<-"Dear manager,
                                            
                                           You receive this notification because you are assigned as part of the %s (%s) as %s.
                                           
                                           New data has been submitted by %s (as %s) for data call '%s' - task '%s' - reporting entity '%s' and shared for your review.
                                           
                                           Submitter notes :
                                           
                                           %s
                                           
                                           Best regards,
                                           The system bot"
          }else{
            body<-"Dear manager,
                                            
                                           You receive this notification because you are assigned as part of the %s (%s) as %s.
                                           
                                           New data has been submitted by %s (as %s) for data call '%s' - task '%s' - reporting entity '%s' and shared for your review.
                                           
                                           Notice that a submission was already provided by this user for this datacall and the present one replaces and cancels the previous one.
                                           
                                           Submitter notes :
                                           
                                           %s
                                           
                                           Best regards,
                                           The system bot"
          }
          
          dcf_managers <- getDBUsers(pool = pool, profile = profile, roles = "manager")
          sent <- sendMessage(subject = sprintf("New data submission for data call '%s' - task '%s' - reporting entity '%s'",
                                                submission$data_call_id, submission$task_id, submission$reporting_entity),
                              body = sprintf(body,
                                             config$dcf$name, config$dcf$context, config$dcf$roles$manager, 
                                             profile$name, config$dcf$roles$submitter, submission$data_call_id, submission$task_id, submission$reporting_entity,
                                             input$message),
                              recipients = as.list(dcf_managers$username),
                              attachment_ids =list(uploadedOriginalDataId,uploadedDataId,uploadedMetadataId ,uploadedReportStandardConformityId,uploadedReportDatacallConsistencyId) ,
                              profile = profile
          )
          
          sendMessage(subject = sprintf("Your submission for data call '%s' - task '%s' - reporting entity '%s' is submitted",
                                        submission$data_call_id, submission$task_id, submission$reporting_entity),
                      body = sprintf("Dear %s,
                      
                                      Thank you for your data deposit.
                      
                                      Your data for data call '%s' - task '%s' - reporting entity '%s' has been successfully submitted to %s and will be reviewed soon.
                                           
                                      Best regards,
                                      The system bot",
                              
                                     profile$name, submission$data_call_id, submission$task_id, submission$reporting_entity, config$dcf$roles$manager),
                      recipients = list(profile$preferred_username),
                      attachment_ids =list(uploadedOriginalDataId,uploadedDataId,uploadedMetadataId ,uploadedReportStandardConformityId,uploadedReportDatacallConsistencyId) ,
                      profile = profile
          )
          
          # progress$set(
          #   message = "Successful data submission", 
          #   detail = sprintf("Data call: %s; Task: %s; Reporting entity: %s", submission$data_call_id, submission$task_id, submission$reporting_entity),
          #   value = 100
          # )
          
          hostess$set(99)
          shinyjs::html(id="wait_detail",html="new label")
        }
        waiter_hide()
      }
      
      observeEvent(input$send,{
        
        dc_folder <- paste0("datacall-",submission$data_call_id, "_task-", submission$task_id, "_for_", submission$reporting_entity)
        dc_folder_id <- store$getWSItemID(parentFolderID = config$workspace_id, itemPath = dc_folder)
        if(is.null(dc_folder_id)){
          submitData(new=TRUE,session,dc_folder,submission,pool,profile,store,config,input)
          submitted<-submitted(TRUE)
        }else{
          showModal(modalDialog(
            title = "Caution a submission was already deposited for this datacall",
            tagList(
              p("Would you do really overwrite your precedent submission by this one ?"),
              p("Click 'Update' to send and overwrite your precedent submission or click 'Cancel' to return to menu without sending your submission.")
            ),
            easyClose = F,
            footer = tagList(
              actionButton(ns("update"),"Yes"),
              actionButton(ns("close4"),"Cancel")
            )
          ))
        }
      })
      
      observeEvent(input$update,{
        removeModal()
        dc_folder <- paste0("datacall-",submission$data_call_id, "_task-", submission$task_id, "_for_", submission$reporting_entity)
        submitData(new=FALSE,session,dc_folder,submission,pool,profile,store,config,input)
        submitted<-submitted(TRUE)
      })
      
      observeEvent(req(!is.null(submitted())),{
        if(submitted()){
          appendTab(inputId = "wizard-tabs",
                    session = parent.session,
                    select=TRUE,
                    tabPanel("6-Thank you", 
                             tagList(
                               p("Your data has been submitted, click to 'Finish' to return to the menu."),
                               #Close
                               actionButton(ns("close3"),"Finish")
                             )
                    )
          )
        }
      })

      #-----------------------------------------------------------------------------------
    }
  )
  
}