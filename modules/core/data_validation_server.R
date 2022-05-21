data_validation_server <- function(id, parent.session, config, profile, pool){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      ns <- session$ns
      #Initialize reactive values
      #-----------------------------------------------------------------------------------
      restart<-reactiveVal(TRUE)
      dataCall<-reactiveVal(FALSE)
      gbOut<-reactiveVal(NULL)
      dcOut<-reactiveVal(NULL)
      taskProperties<-reactiveVal(NULL)
      loadedData<-reactiveVal(NULL)
      
      #Initialize module content (Home page)
      #-----------------------------------------------------------------------------------
      #HOME
      observeEvent(restart(),{
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
      #RESTART FUNCTION
      restartProcess<-function(){
        restart<-restart(TRUE)
        Sys.unsetenv("DATA_CALL_YEAR")
        output$dataCallMessage<-renderUI({NULL})
      }
      
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
      })
      #GO BACK HOME FROM TAB 4
      observeEvent(input$close2,{
        restartProcess()
      })
      #GO BACK HOME FROM TAB 6
      observeEvent(input$close3,{
        restartProcess()
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
        if(!is.null(profile$reporting_entities)) {
          if(profile$reporting_entities != ""){
            if(config$dcf$reporting_entities$name %in% c("country", "flagstate")){
              selectizeInput(ns("reporting_entity"), label = "Reporting entity", selected = NULL, multiple = FALSE, 
               choices = {
                 ref_entity <- getReportingEntityCodes(config)
                 ref_entity <- ref_entity[ref_entity$code %in% profile$reporting_entities,]
                 entity_choices <- ref_entity$code
                 setNames(entity_choices, ref_entity$label)
               },options = list( 
                 render = I("{
                            item: function(item, escape) {
                              var icon_href = 'https://countryflagsapi.com/png/'+item.value.toLowerCase();
                              return '<div><img src=\"'+icon_href+'\" height=16 width=32/> ' + item.label + '</div>'; 
                            },
                            option: function(item, escape) { 
                              var icon_href = 'https://countryflagsapi.com/png/'+item.value.toLowerCase();
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
                  ref_entity <- ref_entity[ref_entity$code %in% profile$reporting_entities,]
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
          fileInput(ns("file"), label = "File to test",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = "Choose file")
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
          }else if(any(endsWith(input$file$datapath,"csv"))){
            data<-readr::read_csv(input$file$datapath,col_types = readr::cols(.default = "c"))
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
        
        showModal(modalDialog(
          title = "Validation in progress",
          "Please wait, validity of your dataset is currently being checked ...",
          easyClose = TRUE,
          footer = NULL
        ))
        data<-loadedData()
        INFO("Read Task '%s' column definitions", taskProperties()$name)
        task_def <- readTaskColumnDefinitions(file = taskRules, format = input$format, config = config, reporting_entity = input$reporting_entity)
        INFO("Validate data")
        out<-validateData(file=data, task_def = task_def, config = config)
        INFO("Successful data validation")
        gbOut<-gbOut(out)
        if(out$valid){
          data<-standardizeNames(file=data,format=input$format,rules=taskRules)
          print(head(as.data.frame(data),2))
          if(input$format=="simplified"){
            #TODO review with @abennici
            type<-switch(input$task,
                         "task-I.2"="catch",
                         "task-II.1"="catch",
                         "task-II.2"="effort")
            data<-simplifiedToGeneric(file=data, type)
            print(head(as.data.frame(data),2))
          }
          loadedData<-loadedData(data)
        }
        removeModal()
        
        print(names(loadedData()))

      })
      #TAB 3 REPORT ROUTINE
      output$globalValidReport<-renderUI({
        req(!is.null(gbOut()))
        out<-gbOut()
        
        output$gbSummary<-DT::renderDT(server = FALSE, {
          x<-unique(subset(out$errors,select="rule"))
          x$status<-"FAILED"
          test<-merge(out$tests,x,by.x="code",by.y="rule",all.x=T,all.y=F)
          test[is.na(test)]<-"PASSED"
          test[startsWith(test$code,"I"),]$status<-"WARNING" 
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
        
        tagList(
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
          fluidRow(
            column(3,style = "border: 1px solid black;",
                   p(strong("Task ID: "),input$task),
                   p(strong("Date of Report : "),Sys.Date())
            ),
            column(6,style = "border: 1px solid black;",
                   p(strong("Task Name: "),taskProperties()$name),
                   p(strong("File : "),input$file$name)
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
          if(out$valid){
            #Next
            actionButton(ns("goSpecValid"),"Next")
          }else{
            #Close
            actionButton(ns("close1"),"Finish")
          }
        )
        
      })
      
      #TAB 4 - CONSISTENCY WITH DATA CALL
      #TAB 4 MANAGER
      #TODO call validateCallRules once we added the tab
      
      observeEvent(input$goSpecValid,{

        taskSupplRules<-taskProperties()$data_call_limited_on
        data<-loadedData()
        showModal(modalDialog(
          title = "Check consistency with the ongoing data call",
          "Please wait, your dataset is currently checked vs. the ongoing data call ...",
          easyClose = TRUE,
          footer = NULL
        ))
        out<-validateCallRules(file = data,rules = taskSupplRules)
        dcOut<-dcOut(out)
        removeModal()
        
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
        
        
        tagList(
          if(out$valid){
            tags$div(shiny::icon(c('check-circle')), "Data is ready to be submitted", style="margin-top:5px;color:green;font-size: 200%")
          }else{
            tags$div(shiny::icon(c('times-circle')), "Data not is not consistent with the current data call", style="margin-top:5px;color:red;font-size: 200%")
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
          if(out$valid){
            #Next
            actionButton(ns("goSend"),"Next")
          }else{
            #Close
            actionButton(ns("close2"),"Finish")
          }
        )
      })
      
      #TAB 5 - SEND DATA
      #TAB 5 MANAGER
      
      #TODO check if user app folder is created, if not create it
      #TODO check if datacall folder is created,
        #if yes (means we have already a submission), ask user if he/she wants to overwrite content
          #if yes: and reupload all new files, + notification of new submission (changes)
          #if no: finish
        #if no, create it, upload new files, and share the folder with regional/global data manager(s) 
          #+ notification of first submission to regional/global data manager(s), maybe by email
          #+ confirmation by email of the submission
      
      #TODO content
        #1. data file
        #2. metadata TODO think of the elements
        #3. report 1: conformity with standards
        #4. report 2: consistency with data calls
        #5. eventual custom message for the data manager (through a web form)
        
      
      observeEvent(input$goSend,{
        appendTab(inputId = "wizard-tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("5-Send Data", 
                           tagList(
                             #Next
                             actionButton(ns("send"),"Send")
                           )
                  )
        )
      })
      
      #TAB 6 - THANK YOU
      #TAB 6 MANAGER
      observeEvent(input$send,{
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
      })

      #-----------------------------------------------------------------------------------
    }
  )
  
}