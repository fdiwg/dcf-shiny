data_validation_server <- function(id, parent.session, config, profile, pool){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      ns <- session$ns
      
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
                                   "Completed"="completed"),
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
          tagList(
          fileInput(ns("file"), label = "File to test",multiple = FALSE,accept = c(".xlsx",".xls",".csv"),buttonLabel = "Choose file"),
          checkboxInput(ns("preview"),label="See preview",value=FALSE)
          )
        }else{
          NULL
        }
      )
      
      output$run_wrapper<-renderUI(
        if(!is.null(input$file)){
          actionButton(ns("run"),"Test Validity")
        }else{
          NULL
        }
      )
      
        output$dataView<-DT::renderDT(server = FALSE, {
          req(input$preview)
          if(input$preview&!is.null(input$file)){
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
      
      observeEvent(input$run,{
        rules<-"https://data.d4science.net/NQZP"
        out<-validate_task(file=input$file$datapath,format=input$format,rules=rules)
        req(out)
        
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
        #output$result<-renderUI(
        appendTab(inputId = "tabs",
                  session = parent.session,
                  select=TRUE,
                  tabPanel("Report", 
          tagList(
            h2("Data Validation Report",align = "center"),
            br(),
            fluidRow(
              column(3,offset = 1,style = "border: 1px solid black;",
                p(strong("Task ID: "),input$task),
                p(strong("Date of Report : "),Sys.Date())
              ),
              column(4,style = "border: 1px solid black;",
                p(strong("Task Name: "),input$task),
                p(strong("File : "),input$file$name)
              ),
              column(3,style = "border: 1px solid black;",
                p(strong("Format : "),input$format),
                p(strong("Approved for Upload : "),span(ifelse(out$valid,"Yes","No"),style = ifelse(out$valid,"color:green","color:red")))
              )
            ),
            br(),
            h3("COMPLIANCE SUMMARY"),
            br(),
            fluidRow(column(4,offset = 4,DTOutput(ns("summary")))),
            h3("DETAILS"),
            fluidRow(column(6,offset = 3,DTOutput(ns("errors"))))
          )
          ))
        #)
      })
        
       # rules<-switch(input$task,
       #               "task_I.2"="https://data.d4science.net/NQZP"
       #               )
      
      #validate_task(file=input$file$datapath,format=input$format,rules=rules)
      
      #-----------------------------------------------------------------------------------
    }
  )
  
}