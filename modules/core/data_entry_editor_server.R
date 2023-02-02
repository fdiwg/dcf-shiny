data_entry_editor_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      store <- components$STORAGEHUB
      
      ns <- session$ns
      
      template_info<-reactiveVal(NULL)
      
      #TASK SELECTOR
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
      
      #REPORTING ENTITY SELECTOR
      output$reporting_entity_wrapper <- renderUI({
        if(!is.null(profile$reporting_entities)) {
          if(all(profile$reporting_entities != "")){
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
                               ref_entity <- ref_entity[ref_entity$code %in% profile$reporting_entities,]
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
      
      output$download_wrapper <- renderUI({
        req(input$task)
        req(input$reporting_entity)
        req(!is.null(template_info()))
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!=""){
          shinydashboard::box(title="Download template",width = 2,
                              downloadButton(ns("template_xlsx"),label="Excel",icon=shiny::icon("file-excel"),style = "padding: 5px 20px; margin: 2px 8px;"),
                              downloadButton(ns("template_csv"),label="CSV",icon=shiny::icon("file-csv"),style = "padding: 5px 20px; margin: 2px 8px;")
          )
        }
      })
      
      observeEvent(c(input$task,input$reporting_entity),{
        req(input$task)
        req(input$reporting_entity)
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!=""){
          task<-getTaskProperties(config,id=input$task)
          taskRules <- task$dsd_ref_url
          taskDef<-readTaskColumnDefinitions(file = taskRules, format = "simplified", config = config, reporting_entity = input$reporting_entity,force=T)
          task_template<-do.call(rbind,lapply(taskDef, function(x){
            id<-x$id
            label<-if(!is.null(x$aliases[[1]])){x$aliases[[1]]}else{x$id}
            mandatory<-!x$na_allowed
            if(!is.null(x$allowed_values)){
              if(length(x$allowed_values)==1){
                default_value<-unlist(x$allowed_values)
                ref<-list(NA)
              }else{
                default_value<-NA
                ref<-list(tibble(code=unlist(x$allowed_values),label=unlist(x$allowed_values)))
              }
            }else{
              default_value<-NA
              ref<-list(if(!is.null(x$ref)){readr::read_csv(x$ref)}else{NA})
            }
            
            data<-tibble(id=id,label=label,mandatory=mandatory,default_value=default_value,ref=ref)
            }))
          template_info<-template_info(task_template)
        }
      })
      
      output$template_csv <- downloadHandler(
        filename = function() { 
          sprintf("template_%s_%s.zip",input$task,input$reporting_entity)
        },
        content = function(filename) {
          list_files<-c()
          tmpdir <- tempdir()
          setwd(tempdir())
          info<-template_info()
          col_names<-info$label
          values_type<-info$default_value
          values_type[is.na(values_type)]<-""
          data_template = data.frame(matrix(nrow = 1, ncol = length(col_names))) 
          names(data_template) = col_names
          data_template[1,]<-values_type
          data_file_path<-sprintf("template_data_%s_%s.csv",input$task,input$reporting_entity)
          write.csv(data_template, data_file_path,row.names = F)
          list_files<-c(list_files,data_file_path)
          with_ref<-subset(info,!is.na(ref))
          
          if(nrow(with_ref)>0){
            for(i in 1:nrow(with_ref)){
              data<-with_ref[i,]
              ref<-data$ref[[1]]
              ref[is.na(ref)]<-""
              ref_file_path<-sprintf("ref_%s.csv",data$id)
              write.csv(ref,ref_file_path,row.names = F)
              list_files<-c(list_files,ref_file_path)
            }
          }
          print(list_files)
          zip(zipfile=filename,files=list_files)
        },
        contentType = "application/zip")
      
      output$template_xlsx <- downloadHandler(
        filename = function() { 
          sprintf("template_%s_%s.xlsx",input$task,input$reporting_entity)
        },
        content = function(filename) {
          info<-template_info()
          col_names<-info$label
          values_type<-info$default_value
          values_type[is.na(values_type)]<-""
          data_template = data.frame(matrix(nrow = 1, ncol = length(col_names))) 
          names(data_template) = col_names
          data_template[1,]<-values_type
          
          ###
          # Create workbook
          wb = createWorkbook()
          addWorksheet(wb, "Data")
          header_st <- createStyle(textDecoration = "Bold")
          
          addStyle(wb, "Data", rows = 1:2, cols = which(col_names%in%subset(info,mandatory==TRUE)$label), gridExpand = TRUE, style = createStyle(fgFill = "#edb458"))
          addStyle(wb, "Data", rows = 1:2, cols = which(col_names%in%subset(info,mandatory==FALSE)$label), gridExpand = TRUE, style = createStyle(fgFill = "#ced4da"))
          setColWidths(wb, "Data", cols = 1:ncol(data_template), widths = "auto")
          writeData(wb, sheet = "Data", x = data_template, startCol = 1,headerStyle = header_st)
          
          with_ref<-subset(info,!is.na(ref))
          
          if(nrow(with_ref)>0){
            for(i in 1:nrow(with_ref)){
              data<-with_ref[i,]
              data$label<-gsub("/","-",data$label)
              ref<-data$ref[[1]]
              ref<-subset(ref,select=-c(uri))
              ref[is.na(ref)]<-""
              col_names<-gsub("/","-",col_names)
              
              addWorksheet(wb, data$label)
              setColWidths(wb, data$label, cols = 1:ncol(ref), widths = "auto")
              writeData(wb, sheet = data$label, x = ref, startCol = 1,headerStyle = header_st)
              protectWorksheet(wb, data$label, protect = TRUE)
              
              dataValidation(wb, "Data", col = which(col_names==data$label), rows = 2, type = "list", value =sprintf("'%s'!$A$2:$A$%s",data$label,nrow(ref)+1),allowBlank = T,showErrorMsg = T)
            }
          }
          # Save workbook
          saveWorkbook(wb, filename, overwrite = TRUE)
        })
      
      #-----------------------------------------------------------------------------------
    }
  )
  
}