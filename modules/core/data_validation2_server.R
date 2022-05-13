data_validation2_server <- function(id, parent.session, config, profile, pool){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      ns <- session$ns
      #Initialize reactive values
      #-----------------------------------------------------------------------------------
      restart<-reactiveVal(TRUE)
      gbValidStatus<-reactiveVal(NULL)
      gbValidReport<-reactiveVal(NULL)
      
      
      #Initialize module content (Home page)
      #-----------------------------------------------------------------------------------
      observeEvent(restart(),{
        req(isTRUE(restart()))
        output$wizard<-renderUI({
          tabsetPanel(id = "tabstest",
                      tabPanel(title="Home",
                               value="home",
                               h2("Welcome on validation and upload module"),
                               p("A descriptive text to guide the user"),
                               p("If you are ready click on 'Start'"),
                               actionButton(ns("start"),"Start")
                      )
          )
        })
      })
      
      #Restart module content (Home page) if 'Finish' button is clicked
      #-----------------------------------------------------------------------------------
      observeEvent(input$close1,{
        restart<-restart(TRUE)
      })
      
      observeEvent(input$close2,{
        restart<-restart(TRUE)
      })
      
      observeEvent(input$close3,{
        restart<-restart(TRUE)
      })
      
      observeEvent(input$close4,{
        restart<-restart(TRUE)
      })
      
      #Select your data panel
      #-----------------------------------------------------------------------------------
      
      output$task_wrapper<-renderUI(
        selectizeInput(ns("task"),
                       label="Task",
                       multiple = F,
                       choices = getTasks(config),
                       selected=NULL,
                       options = list(
                         placeholder = "Please select a task",
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        )
      )
      
      output$format_wrapper<-renderUI(
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
      )
      
      output$file_wrapper<-renderUI(
        if(!is.null(input$format))if(input$format!=""){
          fileInput(ns("file"), label = "File to test",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = "Choose file")
        }else{
          NULL
        }
      )
      
      output$goPreview_wrapper<-renderUI(
        if(!is.null(input$file)){
          actionButton(ns("goPreview"),"Next")
        }else{
          NULL
        }
      )
      
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
      

      
      output$dataView<-DT::renderDT(server = FALSE, {
        req(input$file)
        if(!is.null(input$file)){
          if(any(endsWith(input$file$datapath,c("xls","xlsx")))){
            data<-read_excel(input$file$datapath)
          }else if(any(endsWith(input$file$datapath,"csv"))){
            data<-readr::read_csv(input$file$datapath)
          }else{
            stop()
          }
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
      
      observeEvent(input$goPreview,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel(
                    title="2-Preview", 
                    value="preview",
                           tagList(
                             h2("Preview"),
                             DTOutput(ns("dataView")),
                             p("Please verify if data display correspond to the data to send. If it is please click 'Next' to submit this file, otherwise click 'Previous' to select a new file."),
                             #Previous
                             actionButton(ns("goData"),"Previous"),
                             #Next
                             actionButton(ns("goGlobValid"),"Next")
                           )
                  )
        )
      })
      
      observeEvent(input$goData,{
        removeTab(inputId = "tabstest", 
                  session = parent.session,
                  target = "preview")
        updateTabsetPanel(inputId = "tabstest", 
                          session = parent.session,
                          selected = "select_data")
      })
      
      observeEvent(gbValidStatus(),{
      
      output$globalValidMessage<-renderUI({
        req(!is.null(gbValidStatus()))
        if(gbValidStatus()){
        p("Report valid")
        }else{
        #Close
        p("Report invalid")
        }
      })
      
      output$globalValidReport<-renderUI(
        if(!is.null(gbValidReport())){
        actionLink(ns("showReport"),"Show report")
        }
      )
      
      output$globalValidBtn<-renderUI({
        req(!is.null(gbValidStatus()))
        if(gbValidStatus()){
          #Next
          actionButton(ns("goDataCall"),"Next")
        }else{
          #Close
          actionButton(ns("close1"),"Finish")
        }
      })
      })
      
      observeEvent(input$goGlobValid,{
        taskProfil<-getTask(config,id=input$task)
        taskRules<-taskProfil$dsd_ref_url
        taskName<-taskProfil$name
        
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("3-Global Validation", 
                           tagList(
                             h2("Validation and report"),
                             uiOutput(ns("globalValidMessage")),
                             uiOutput(ns("globalValidReport")),
                             uiOutput(ns("globalValidBtn"))
                           )
                  )
        )
        
        showModal(modalDialog(
          title = "Generic Validation in progress",
          "please wait, your dataset is currently checked for validation ...",
          easyClose = TRUE,
          footer = NULL
        ))
        out<-validate_task(file=input$file$datapath,format=input$format,rules=rules)
        gbValidStatus<-gbValidStatus(out$valid)
        removeModal()
        
        output$summary<-DT::renderDT(server = FALSE, {
        x<-unique(subset(out$errors,select="rule"))
        x$status<-"FAIL"
        test<-merge(out$tests,x,by.x="code",by.y="rule",all.x=T,all.y=F)
        test[is.na(test)]<-"PASS"
        test[startsWith(test$code,"I"),]$status<-"WARNING" 
        test<-subset(test,select=-c(code))
        test$icon<-ifelse(test$status=="PASS",paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse=""),
                          ifelse(test$status=="WARNING",paste0(tags$span(shiny::icon("exclamation-triangle"), title = "Warning", style = "color:orange;"), collapse=""),
                                 paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")))
        
        DT::datatable(
          test, 
          rownames = NULL,
          escape = FALSE,
          colnames = c('','',''), 
          options = list(dom = 't',
                         ordering=F,
                         columnDefs = list(list(className = 'dt-center', targets = 1:2))))%>%
          formatStyle("status",target = 'row',backgroundColor = styleEqual(c("PASS","WARNING","FAIL"), c("#d3f4cc","#f5dfc0","#ffbfc1")))%>%
          formatStyle("status", fontWeight = 'bold',color= styleEqual(c("PASS","WARNING","FAIL"), c("green","orange","red")))
        
      })
      
      output$errors<-DT::renderDT(server = FALSE, {
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


      report_content<-fluidPage(
                           h2("Data Validation Report",align = "center"),
                           br(),
                           fluidRow(
                             column(3,style = "border: 1px solid black;",
                                    p(strong("Task ID: "),input$task),
                                    p(strong("Date of Report : "),Sys.Date())
                             ),
                             column(6,style = "border: 1px solid black;",
                                    p(strong("Task Name: "),taskName),
                                    p(strong("File : "),input$file$name)
                             ),
                             column(3,style = "border: 1px solid black;",
                                    p(strong("Format : "),input$format),
                                    p(strong("Approved for Upload : "),span(ifelse(out$valid,"Yes","No"),style = ifelse(out$valid,"color:green","color:red")))
                             )
                           ),
                           br(),
                           h3("VALIDITY SUMMARY"),
                           br(),
                           fluidRow(column(8,offset = 2,DTOutput(ns("summary")))),
                           h3("DETAILS"),
                           fluidRow(column(10,offset = 1,DTOutput(ns("errors"))))
                         )
      print(sprintf("Length of report content : %s",length(report_content)))
      
      gbValidReport<-gbValidReport(report_content)
        
      })
  
  observeEvent(input$showReport,{
    showModal(modalDialog(
      title = "Generic Validation Report",
      gbValidReport(),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
      
      observeEvent(input$goDataCall,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("4-Data Call", 
                           tagList(
                             h2("Select a data call"),
                             #Next
                             actionButton(ns("goSpecValid"),"Next"),
                             #Close
                             actionButton(ns("close2"),"Finish")
                           )
                  )
        )
      })
      
      observeEvent(input$goSpecValid,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("5-Specific validation", 
                           tagList(
                             h2("Specific validation"),
                             #Next
                             actionButton(ns("goSend"),"Next"),
                             #Close
                             actionButton(ns("close3"),"Finish")
                           )
                  )
        )
      })
      
      observeEvent(input$goSend,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("6-Send Data", 
                           tagList(
                             h2("Submit your data"),
                             #Next
                             actionButton(ns("send"),"Send")
                           )
                  )
        )
      })
      
      observeEvent(input$send,{
        appendTab(inputId = "tabstest",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("7-Thank you", 
                           tagList(
                             h2("Your data are submited"),
                             p("Your data are now send click to 'Finish' to return to the menu."),
                             #Close
                             actionButton(ns("close4"),"Finish")
                           )
                  )
        )
      })

      #-----------------------------------------------------------------------------------
    }
  )
  
}