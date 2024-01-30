data_entry_editor_server <- function(id, parent.session, config, profile, components,reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      store <- components$STORAGEHUB
      
      ns <- session$ns
      
      template_info<-reactiveVal(NULL)
      empty_row<-reactiveVal(NULL)
      current_data<-reactiveVal(NULL)
      ready<-reactiveVal(FALSE)
      menu_tabs<-reactiveVal(c())
      
      output$menu<-renderUI({

        tabBox(id = "tabbox",title=NULL,height="600px",width = "100%",
               tabPanel(title=tagList(icon("gear"),"Settings"),
                        value="tab_settings",
                          div(
                            uiOutput(ns("task_wrapper")),
                            uiOutput(ns("reporting_entity_wrapper")),
                            uiOutput(ns("format_wrapper")),
                            uiOutput(ns("run_wrapper"))
                          )
                        )
               )
      })
      
      observeEvent(template_info(),{
        req(!is.null(template_info()))
        
        info<-template_info()
        
        if("tab_editor"%in%menu_tabs()){
           removeTab(inputId = "tabbox",
                     session = parent.session,
                     target = "tab_editor")
         }

         tabs_list<-unique(c(menu_tabs(),"tab_editor"))
         menu_tabs<-menu_tabs(tabs_list)
        
        appendTab(inputId = "tabbox",
                  session = parent.session,
                  select=TRUE,
          tabPanel(title=tagList(icon("edit"),"Editor"),
                   value="tab_editor",
                     uiOutput(ns("buttons_wrapper")),
                     br(),
                     uiOutput(ns("table_wrapper"))
                   
          )
        )
        
        if(any(!is.na(info$ref))){
          
          if("tab_referentials"%in%menu_tabs()){
            removeTab(inputId = "tabbox",
                      session = parent.session,
                      target = "tab_referentials")
          }
          
          tabs_list<-unique(c(menu_tabs(),"tab_referentials"))
          menu_tabs<-menu_tabs(tabs_list)
          
          appendTab(inputId = "tabbox",
                    session = parent.session,
                    select=FALSE,
            tabPanel(title=tagList(icon("search"),"Referentials"),
                     value ="tab_referentials",
                     div(
                       uiOutput(ns("ref_to_show_wrapper")),
                       uiOutput(ns("display"))
                     )
            )
          )
        }
        
        if("tab_templates"%in%menu_tabs()){
          removeTab(inputId = "tabbox",
                    session = parent.session,
                    target = "tab_templates")
        }
         
        tabs_list<-unique(c(menu_tabs(),"tab_templates"))
        menu_tabs<-menu_tabs(tabs_list)
         
        appendTab(inputId = "tabbox",
                  session = parent.session,
                  select=FALSE,
                  tabPanel(title=tagList(icon("download"),"Templates"),
                           value="tab_templates",
                           uiOutput(ns("download_wrapper"))
                  )
        )
        
      })
      
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
      
      observeEvent(input$task,{
        req(input$task)
        
        output$format_wrapper<-renderUI({
          if(input$task!=""){
            selectizeInput(ns("format"),
                           label="Data format",
                           multiple = F,
                           choices = getTaskFormats(config,id=input$task),
                           selected=NULL,
                           options = list(
                             placeholder = "Please select a format",
                             onInitialize = I('function() { this.setValue(""); }')
                           )
            )
          }
        })
      })
      
      output$run_wrapper <- renderUI({
        req(input$task)
        req(input$reporting_entity)
        req(input$format)
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!="")if(!is.null(input$format))if(input$format!=""){

          withBusyIndicatorUI(
            actionButton(ns("run"),title="Run selection",label="Run selection")
          )
        }
      })
      
      output$download_wrapper <- renderUI({
        req(input$task)
        req(input$reporting_entity)
        req(input$format)
        req(!is.null(template_info()))
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!="")if(!is.null(input$format))if(input$format!=""){
          shinydashboard::box(title="Download template",width = 2,
                              downloadButton(ns("template_xlsx"),label="Excel",icon=shiny::icon("file-excel"),style = "padding: 5px 20px; margin: 2px 8px;"),
                              downloadButton(ns("template_csv"),label="CSV",icon=shiny::icon("file-csv"),style = "padding: 5px 20px; margin: 2px 8px;")
          )
        }
      })
      
      output$buttons_wrapper <- renderUI({
        req(input$task)
        req(input$reporting_entity)
        req(input$format)
        req(!is.null(template_info()))
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!="")if(!is.null(input$format))if(input$format!=""){

          div(
            actionButton(ns("new_data"),title="Create a new data table",label="New data",icon=icon("file-circle-plus"),class = "btn-light"),
            actionButton(ns("import_data"),title="Import an existing data table",label="Import data",icon=icon("folder-open"),class = "btn-light"),
            uiOutput(ns("save_wrapper"))
            )

        }
      })
      
      output$save_wrapper <- renderUI({
        req(input$task)
        req(input$reporting_entity)
        req(input$format)
        req(ready())
        req(!is.null(current_data()))
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!="")if(!is.null(input$format))if(input$format!=""){
          downloadButton(ns("save_data"),title="Save your data",label="Save data",icon=shiny::icon("save"),class = "btn-success")
        }
      })
      
      observeEvent(input$new_data,{
        req(input$new_data)
          ready<-ready(TRUE)
      })
      
      observeEvent(input$import_data,{
        req(input$import_data)
          ready<-ready(FALSE)
          showModal(
            modalDialog(
              title = "",
              fileInput(ns("file"), label = "File to edit",multiple = FALSE,accept = c(".csv",".zip"),buttonLabel = "Choose file"),
              easyClose = TRUE, footer = NULL,size="s" 
            )
          )
              
          })
      
      observeEvent(input$file,{
        req(input$file)
        info<-template_info()
        if(!is.null(input$file)){
          removeModal()
          if(any(endsWith(input$file$datapath,"csv"))){
            data_to_load<-readr::read_csv(input$file$datapath,col_types = readr::cols(.default = "c"))
          }else if(any(endsWith(input$file$datapath,"zip"))){
            files<-zip_list(input$file$datapath)
            unzip(input$file$datapath,files=c(files$filename[1]),exdir = dirname(input$file$datapath))
            target_file<-file.path(dirname(input$file$datapath),files$filename[1])
            if(any(endsWith(target_file,c("xls","xlsx")))){
              data_to_load<-read_excel(target_file,col_types = "text")
            }else if(any(endsWith(target_file,"csv"))){
              data_to_load<-readr::read_csv(target_file,col_types = readr::cols(.default = "c"))
            }else{
              stop()
            }
          }else{
            stop()
          }
          if(all(names(data_to_load)==info$name)){
            data_to_lead<-as.data.frame(data_to_load)
            if(any(!is.na(info$ref))){
              cols<-which(!is.na(info$ref))
              correct_order<-names(data_to_load)
              data_to_load$row_order<-1:nrow(data_to_load)
              for(col in cols){
                ref<-info[col,]$ref[[1]]
                ref<-as.data.frame(ref)
                ref<-unique(subset(ref,code%in%unique(data_to_load[,col]),select=c(code,label)))
                
                data_to_load<-merge(data_to_load,ref,by.x=names(data_to_load[col]),by.y="code",all.x=T,sort=F)
                data_to_load<-data_to_load[,-1]
                names(data_to_load)[ncol(data_to_load)]<-correct_order[col]
                data_to_load<-subset(data_to_load,select=c(correct_order,"row_order"))
              }
              data_to_load <- data_to_load[order(data_to_load$row_order), ]
              data_to_load <-subset(data_to_load,select=-c(row_order))
            }
            names(data_to_load)<-info$label
            current_data<-current_data(data_to_load)
            ready<-ready(TRUE)
          }else{
            stop("NOT CORRECT COLUMNS DEFINITIONS")
          }
        }
      })
      
      observeEvent(input$run,{
        req(input$task)
        req(input$reporting_entity)
        req(input$format)
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!="")if(!is.null(input$format))if(input$format!=""){
          reset<-FALSE
          if(!is.null(input$table)){
            reset<-TRUE
            showModal(
              modalDialog(
                title = "Warning unsaved data will be delete",
                p("The refresh of the selection will reset unsaved data."),
                p("Do you confirm to reset selection ?"),
                actionButton(ns("reset_ok"), "Confirm"),
                actionButton(ns("reset_cancel"), "Cancel", style = "float:right;"),
                easyClose = FALSE, footer = NULL,size="s" 
              )
            )
          }
          req(reset==FALSE)
          withBusyIndicatorServer(ns("run"), {
            task<-getTaskProfile(config,id=input$task)
            taskRules <- task$dsd_ref_url
            taskDef<-readTaskDefinition(file = taskRules)
            format_spec = taskDef$formats[[input$format]]$spec
            
            #TODO process reporting_entity in format_spec
            task_template = buildTemplate(format_spec)
            
            template_info<-template_info(task_template)
            
            info<-template_info()
            col_names<-info$label
            values_type<-info$default_value
            values_type[is.na(values_type)]<-""
            if(any(!is.na(info$ref))){
              cols<-which(!is.na(info$ref))
              for(col in cols){
                values_type[col]<-NA_character_
              }
            }
            data_template = data.frame(matrix(nrow = 1, ncol = length(col_names))) 
            names(data_template) = col_names
            data_template[1,]<-values_type
            
            empty_row<-empty_row(data_template)
            current_data<-current_data(data_template[rep(seq_len(nrow(data_template)), 10), ])
          })
        }
      })
      
      observeEvent(input$reset_cancel,{
        removeModal()
      })
      
      observeEvent(input$reset_ok,{
        removeModal()
        req(input$task)
        req(input$reporting_entity)
        req(input$format)
        if(!is.null(input$task))if(input$task!="")if(!is.null(input$reporting_entity))if(input$reporting_entity!="")if(!is.null(input$format))if(input$format!=""){
          withBusyIndicatorServer(ns("run"), {
            task<-getTaskProfile(config,id=input$task)
            taskRules <- task$dsd_ref_url
            taskDef<-readTaskDefinition(file = taskRules)
            format_spec = taskDef$formats[[input$format]]$spec
            
            #TODO process reporting_entity in format_spec
            task_template = buildTemplate(format_spec)
            
            template_info<-template_info(task_template)
            
            info<-template_info()
            col_names<-info$label
            values_type<-info$default_value
            values_type[is.na(values_type)]<-""
            if(any(!is.na(info$ref))){
              cols<-which(!is.na(info$ref))
              for(col in cols){
                values_type[col]<-NA_character_
              }
            }
            data_template = data.frame(matrix(nrow = 1, ncol = length(col_names))) 
            names(data_template) = col_names
            data_template[1,]<-values_type
            
            empty_row<-empty_row(data_template)
            current_data<-current_data(data_template[rep(seq_len(nrow(data_template)), 10), ])
          })
        }
      })
      
      output$template_csv <- downloadHandler(
        filename = function() { 
          sprintf("template_%s_%s_%s.zip",input$task,input$reporting_entity,input$format)
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
          data_file_path<-sprintf("template_data_%s_%s_%s.csv",input$task,input$reporting_entity,input$format)
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
          sprintf("template_%s_%s_%s.xlsx",input$task,input$reporting_entity,input$format)
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
      
      output$save_data <- downloadHandler(
        filename = function() { 
          sprintf("data_%s_%s_%s.csv",input$task,input$reporting_entity,input$format)
        },
        content = function(filename) {
          data_to_save<-hot_to_r(input$table)
          data_to_save<-as.data.frame(data_to_save)
          info<-template_info()
          names(data_to_save)<-info$name
          if(any(!is.na(info$ref))){
            cols<-which(!is.na(info$ref))
            correct_order<-names(data_to_save)
            data_to_save$row_order<-1:nrow(data_to_save)
            for(col in cols){
              ref<-info[col,]$ref[[1]]
              ref<-as.data.frame(ref)
              ref<-unique(subset(ref,label%in%unique(data_to_save[,col]),select=c(code,label)))
              
              data_to_save<-merge(data_to_save,ref,by.x=names(data_to_save[col]),by.y="label",all.x=T,sort=F)
              data_to_save<-data_to_save[,-1]
              names(data_to_save)[ncol(data_to_save)]<-c(correct_order[col])
              data_to_save<-subset(data_to_save,select=c(correct_order,"row_order"))
              
            }
            data_to_save <- data_to_save[order(data_to_save$row_order), ]
            data_to_save <-subset(data_to_save,select=-c(row_order))
          }
          
          write.csv(data_to_save, filename,row.names = F)
        })
      
      
      observeEvent(current_data(),{
      output$table<-renderRHandsontable({
        req(!is.null(current_data()))
        req(ready())
        data<-current_data()
        row.names(data)<-1:nrow(data)
        info<-template_info()
        editable_table<-rhandsontable(data) %>%
               hot_context_menu(allowRowEdit = T, allowColEdit = F)
        
         if(any(!info$editable)){
           cols<-which(info$editable==FALSE)
           for(col in cols){
             editable_table <- hot_col(editable_table, col = col, readOnly = TRUE)
           }
         }
        
        if(any(!is.na(info$ref))){
          cols<-which(!is.na(info$ref))
          for(col in cols){
            withref<-info[col,]
            ref<-withref$ref[[1]]
            ref_code<-ref$label

            print(head(ref_code))
            editable_table <- hot_col(editable_table,col=col, type = "dropdown", source = ref_code)
          }
        }
        
        return(editable_table)
        
      
      })
      })
      
      output$table_wrapper<-renderUI({
        req(!is.null(current_data()))
        req(ready())
        info<-template_info()
        div(
          div(
            rHandsontableOutput(ns("table"))
          ),
          br(),
          div(
            column(1,
                   actionButton(ns("add_row"),title="Add new row(s) to the table",label="Add row",icon=icon("plus"),class = "btn-info")),
            column(1,
                   numericInput(ns("nb_add_row"),label=NULL,min=1,max=100,step=1,value=1))
          
          )
        )
        })
      
      output$ref_to_show_wrapper<-renderUI({
        req(!is.null(template_info()))
        info<-template_info()
        req(any(!is.na(info$ref)))
    
        withref<-info[!is.na(info$ref),]$label
        withref_index<-which(!is.na(info$ref))
        selectizeInput(ns("ref_to_show"),
                       label="Valid values for column :",
                       multiple = F,
                       choices = setNames(withref_index,withref))
        
      })
      
      observeEvent(input$ref_to_show,{
        info<-template_info()
        ref<-info[as.numeric(input$ref_to_show),]$ref[[1]]
        
        output$display_table<-DT::renderDT(
          datatable(
            ref,
            escape=FALSE,rownames=FALSE,
            options=list(
              pageLength = 10,
              searching = TRUE,
              autoWidth = T)
          )%>% formatStyle('label',backgroundColor ='#CEF3D6' )
        )
        
        output$display<- renderUI({
          DT::dataTableOutput(ns("display_table"))
        })
        
      })
      
      observeEvent(input$add_row, {
        
        new_row<-empty_row()
        
        for (i in 1 : input$nb_add_row){
          
          if(i==1){
            last_data <- hot_to_r(input$table)
          }else {
            last_data <- new_data
          }
          new_data <- rbind(last_data,new_row)
          }
          
          current_data<-current_data(new_data)
        
      })

      
      #-----------------------------------------------------------------------------------
    }
  )
  
}