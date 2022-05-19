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
          tabsetPanel(id = "tabstest",
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
        removeTab(inputId = "tabstest", 
                  session = parent.session,
                  target = "preview")
        updateTabsetPanel(inputId = "tabstest", 
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
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="1-Select your data", 
                    value ="select_data",
                    tagList(
                      h2("Select your data"),
                      uiOutput(ns("task_wrapper")),
                      uiOutput(ns("dataCallMessage")),
                      uiOutput(ns("format_wrapper")),
                      uiOutput(ns("file_wrapper")),
                      #Next
                      uiOutput(ns("goPreview_wrapper"))
                    )
                  )
        )
        removeTab(inputId = "tabstest", 
                  session = parent.session,
                  target = "home")
        updateTabsetPanel(inputId = "tabstest", 
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
      #TAB 1 FORMAT SELECTOR
      output$format_wrapper<-renderUI({
        if(!is.null(input$task))if(input$task!=""){
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
        taskProfil<-getTaskProperties(config,id=input$task)
        taskProperties<-taskProperties(taskProfil)
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
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="2-Preview", 
                    value="preview",
                           tagList(
                             h2("Preview"),
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
        taskRules<-taskProperties()$dsd_ref_url
        
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(title="3-Conformity with standards", 
                           value="standard_validation",
                           tagList(
                             h2("Conformity with standard format and content"),
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
        out<-validateData(file=data,format=input$format,rules=taskRules)
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
            tags$span(shiny::icon(c('check-circle')), "Data is valid", style="color:green;font-size: 300%")
          }else{
            tags$span(shiny::icon(c('times-circle')), "Data is invalid", style="color:red;font-size: 300%")
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
      observeEvent(input$goSpecValid,{
        
          taskSupplRules<-taskProperties()$data_call_limited_on
          data<-loadedData()
          showModal(modalDialog(
            title = "Check consistency with the ongoing data call",
            "Please wait, your dataset is currently checked vs. the ongoing data call ...",
            easyClose = TRUE,
            footer = NULL
          ))
          out<-validateCallRules(data,taskSupplRules)
          dcOut<-dcOut(out)
          removeModal()
          
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(title="4-Consistency with data Call", 
                           value="specific_validation",
                           tagList(
                             h2("Consistency with data call"),
                             uiOutput(ns("callValidReport"))
                           )
                  )
        )

      })
      #TAB 4 REPORT ROUTINE
      output$callValidReport<-renderUI({
        req(!is.null(dcOut()))
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
            tags$span(shiny::icon(c('check-circle')), "Data is ready to be submitted", style="color:green;font-size: 300%")
          }else{
            tags$span(shiny::icon(c('times-circle')), "Data not is not consistent with the current data call", style="color:red;font-size: 300%")
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
      observeEvent(input$goSend,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("5-Send Data", 
                           tagList(
                             h2("Submit your data"),
                             #Next
                             actionButton(ns("send"),"Send")
                           )
                  )
        )
      })
      
      #TAB 6 - THANK YOU
      #TAB 6 MANAGER
      observeEvent(input$send,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("6-Thank you", 
                           tagList(
                             h2("Your data has been submitted"),
                             p("Your data has been sent, click to 'Finish' to return to the menu."),
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