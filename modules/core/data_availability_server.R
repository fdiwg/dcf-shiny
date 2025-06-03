data_availability_server <-function(id, parent.session, config, profile, components,reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #SH <- components$STORAGEHUB
      pool <- components$POOL
      
      #getDataTaskTablename
      getDataTaskTablename <- function(task_id){
        table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
        return(table_id)
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
      #task_folders <- SH$listWSItems(config$dataspace_id)
      reporting_entities<-config$dcf$reporting_entities$codelist_ref$code
      
      data_tasks<-lapply(setNames(getTasks(config,withId=TRUE),getTasks(config,withId=TRUE)),function(x){
        getDataTaskDBData(pool, x)
      })
      
      data<-reactiveVal(NULL)
      data_s<-reactiveVal(NULL)
      #dataAvailable<-reactiveVal(ifelse(length(task_folders)==0,FALSE,TRUE))
      dataAvailable<-reactiveVal(ifelse(all(sapply(data_tasks, is.null)),FALSE,TRUE))
      #dataAvailable<-reactiveVal(TRUE)
      
      pretty_seq<-function(x){
        
        break_list<-which(c(1,diff(x)) != 1)
        done<-c()
        new_vec<-c()
        
        if(length(break_list>0)){
          for(i in break_list){
            
            target<-x[1:i-1]
            
            target<-setdiff(target,done)
            
            done<-c(done,target)
            
            min_v<-min(target)
            
            max_v<-max(target)
            
            seq_v<-if(min_v!=max_v){paste0(min_v,"-",max_v)}else{as.character(min_v)}
            
            new_vec<-c(new_vec,seq_v)
          }
          
          remaining<-setdiff(x,done)
          new_vec<-c(new_vec,remaining)
        }else{
          min_v<-min(x)
          max_v<-max(x)
          new_vec<-if(min_v!=max_v){paste0(min_v,"-",max_v)}else{as.character(min_v)}
        }
        return(new_vec)
      }
      
      output$summary_content<-renderUI({
        tagList(
          fluidRow(
            # div(
            #   class = "col-md-2",
            #   withBusyIndicatorUI(
            #     actionButton(ns("summaryBtn"),"Compute summary")
            #   )
            # ),
            div(
              class = "col-md-2",
              uiOutput(ns("entities_selector_s"))
            ),
            div(
              class = "col-md-2",
              uiOutput(ns("stat_selector_s"))
            ),
            div(
              class = "col-md-2",
              uiOutput(ns("download_wrapper"))
            )
          ),
            uiOutput(ns("heatmap_s_wrapper"))
          
        )
      })
      
      output$by_task_content<-renderUI({
        if(dataAvailable()){
          tagList(
            fluidRow(
              div(
                class = "col-md-2",
                uiOutput(ns("task_selector"))
              ),
              div(
                class = "col-md-2",
                uiOutput(ns("entities_selector"))
              ),
              div(
                class = "col-md-8",
                uiOutput(ns("download_task_wrapper"))
              )
            ),
            uiOutput(ns("heatmap_wrapper"))
          )}else{
            p("(No data available)")
          }
      })
      
      output$menu<-renderUI({
        
        tabBox(id = "tabbox_availability",title=NULL,height="600px",width = "100%",
               tabPanel(title=tagList(icon("clipboard")," Summary"),
                        value="tab_summary",
                        uiOutput(ns("summary_content"))
               ),
               tabPanel(title=tagList(icon("list")," By Task"),
                        value="tab_by_task",
                        uiOutput(ns("by_task_content"))
               )
        )
      })
      
      
      
      #status selector
      output$entities_selector <- renderUI({
        checkboxInput(ns("limit_entities"), "Limit to entities with data", TRUE)
      })
      
      output$entities_selector_s <- renderUI({
        checkboxInput(ns("limit_entities_s"), "Limit to entities with data", dataAvailable())
      })
      
      output$stat_selector_s<-renderUI({
        selectizeInput(ns("stat_s"),
                       label="Statistic",
                       multiple = F,
                       choices = c("Oldest available year"="min_year",
                                   "Most recent available year"="max_year",
                                   "Covered period"="period",
                                   "Available years"="available_years",
                                   "Number of years"="nb_year",
                                   "Number of records"="nb_record"),
                       selected="period"
        )
      })
      
      #Task selector
      output$task_selector<-renderUI({
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
      
      #observeEvent(input$summaryBtn,{
      observeEvent(data_tasks,{
        #withBusyIndicatorServer(ns("summaryBtn"), {
          if(dataAvailable()){
            
            tasks<-getTasks(config,withId=T) 
              
            summary<-do.call("rbind",lapply(getTasks(config,withId=TRUE),function(x){
            # items <- SH$listWSItems(parentFolderID = subset(task_folders,name==x)$id)
            # last_modification_time <- max(items$lastModificationTime)
            # items <- items[items$lastModificationTime == last_modification_time,]
            # items <- items [endsWith(items$name,"_db_new.csv"),]
            # item<-SH$downloadItem(item = items, wd = tempdir())
            # file<-readr::read_csv(item)
               
            #file<-getDataTaskDBData(pool, x)

            file<-data_task<-data_tasks[[x]]
            if(!is.null(file)){
              colnames(file)[colnames(file)==config$dcf$reporting_entities$name] = "reporting_entity"
              file<-file%>%
                group_by(reporting_entity)%>%
                summarise(period=paste0("first:",year(min(time_end,na.rm=T)),"- last:",year(max(time_end,na.rm=T))),
                          min_year=as.character(year(min(time_end,na.rm=T))),
                          max_year=as.character(year(max(time_end,na.rm=T))),
                          nb_year=as.character(length(unique(year(time_end)))),
                          nb_record=as.character(length(reporting_entity)),
                          available_years=paste0(pretty_seq(sort(unique(year(time_end)))),collapse=";"))%>%
                arrange(desc(reporting_entity))%>%
                ungroup()%>%
                #mutate(task=x)
                mutate(task=gsub(": ",":\n",names(tasks[tasks==x])))
              colnames(file)[colnames(file)=="reporting_entity"] = config$dcf$reporting_entities$name
            }else{
              WARN("No data available for task '%s'", x)
            }
            return(file)
          }))
            
        }else{
          summary<-do.call("rbind",lapply(getTasks(config,withId=TRUE),function(x){
            
            file<-data.frame(reporting_entity="",
                             period="(no data)",
                             min_year="(no data)",
                             max_year="(no data)",
                             nb_year="0",
                             nb_record="0",
                             available_years="(no data)",
                             task=x)%>%
              arrange(desc(reporting_entity))%>%
              ungroup()
            colnames(file)[colnames(file)=="reporting_entity"] = config$dcf$reporting_entities$name
            file
          })
          )
            
        }
        data_s<-data_s(summary)
        #})
      })
      
      output$download_wrapper<-renderUI({
        req(data_s())
        if(nrow(data_s())>0){
        downloadButton(ns("download"),label="Download summary",icon=shiny::icon("file-excel"),style = "padding: 5px 20px; margin: 2px 8px;")
        }
      })
      
      output$download_task_wrapper<-renderUI({
        req(data())
        req(req(input$task))
        
        choices<-getTaskFormats(config,id=input$task)
        
        if(nrow(data())>0){
          box(
            title="Download dataset",
          div(
            if(length(choices)>1){
              selectizeInput(ns("format"),
                             label="Export format",
                             multiple = F,
                             choices = choices ,
                             selected=1,
                             options = list(
                               placeholder = "Please select an export format",
                               onInitialize = I('function() { this.setValue(""); }')
                             )
              )
            }else{
              disabled(
                selectizeInput(ns("format"),
                               label="Export format",
                               multiple = F,
                               choices = choices,
                               selected=1
                )
              )
            },
            checkboxInput(ns("with_codelist_labels"), "Enrich with codelists labels", value = FALSE),
            checkboxInput(ns("with_col_aliases"), "Use column aliases names", value = FALSE),
            downloadButton(ns("download_task"),label="Download data",icon=shiny::icon("download"),style = "padding: 5px 20px; margin: 2px 8px;")
          )
          )
        }
      })
      
      output$download_task <- downloadHandler(
        filename = function() { 
          sprintf("data_%s_%s.csv",input$task,Sys.Date())
        },
        content = function(filename) {
          req(nrow(data())>0)
          req(!is.null(input$with_col_aliases))
          req(!is.null(input$with_codelist_labels))
          req(!is.null(input$format))
          
          INFO("Export task %s data", input$task)
          INFO("Export options:")
          INFO("* Format: %s", input$format)
          INFO("* Column aliases: %s", input$with_col_aliases)
          INFO("* Codelist labels: %s", input$with_codelist_labels)
          
          #inherit dataset
          target_data<-data()
          
          #inherit format 
          task <- getReportingTask(config, id = input$task)
          format_spec = task$formats[[input$format]]$spec
          
          if(input$format!="generic"){
            INFO("Transforming to 'simplified' format")
            target_data<-genericToSimplified(target_data)
            INFO("Successful transformation from 'generic' to 'simplified' format!")
          }
          
          if(input$with_codelist_labels){
            #enrich with codelist labels
            for(col in  names(target_data)){
              column_spec = format_spec$getColumnSpecByName(col)
              if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("month")
              if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("quarter")
              if(!is.null(column_spec)){
                if(column_spec$hasCodelist() && any(sapply(column_spec$rules, is, "vrule_codelist"))){
                  cl_rule = column_spec$rules[sapply(column_spec$rules, is, "vrule_codelist")][[1]]
                  ref<-subset(cl_rule$ref_data,select=c(code,label))
                  #manage column aliases if checked
                  alias = col
                  if(input$with_col_aliases) if(length(column_spec$aliases)>0) alias = column_spec$aliases[[1]]
                  names(ref)<-c(sprintf("%s [code]",alias),sprintf("%s [label]",alias))
                  names(target_data)[names(target_data) == col] <-sprintf("%s [code]",alias)
                  target_data<-merge(target_data,ref,all.x=T,all.y=F)
                }else{
                  #manage column aliases if checked
                  if(input$with_col_aliases) if(length(column_spec$aliases)>0) {
                    names(target_data)[names(target_data) == col] <- column_spec$aliases[[1]]
                  }
                }
              }
            }
          }else{
            for(col in  names(target_data)){
              column_spec = format_spec$getColumnSpecByName(col)
              if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("month")
              if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("quarter")
              if(!is.null(column_spec)){
                #manage column aliases if checked
                if(input$with_col_aliases) if(length(column_spec$aliases)>0) {
                  names(target_data)[names(target_data) == col] <- column_spec$aliases[[1]]
                }
              }
            }
          }
          
          #reorder
          target_data = do.call("cbind", lapply(format_spec$column_specs, function(column_spec){
            out = NULL
            col = column_spec$name
            if(column_spec$name %in% c("month", "quarter")) col = "period"
            if(input$with_col_aliases) if(length(column_spec$aliases)>0){
              col = column_spec$aliases[[1]]
            }
            has_cl = column_spec$hasCodelist() && any(sapply(column_spec$rules, is, "vrule_codelist"))
            reorder = if(has_cl){
              startsWith(colnames(target_data), col) #if enrichable with labels, we should get the code/label columns
            }else{
              colnames(target_data) == col #if not enrichable with labels, we look for identity
            }
            
            if(length(reorder)>0){
              columns = colnames(target_data)[reorder]
              out = target_data[,columns]
              if(length(columns)==1){
                out = data.frame(col = out)
                colnames(out) = columns
              }
            }
            if(column_spec$name == "month") colnames(out)[colnames(out)=="period"] = "month"
            if(column_spec$name == "quarter") colnames(out)[colnames(out)=="period"] = "quarter"
            out
          }))
          INFO("Successful data export!")
          readr::write_csv(target_data,file = filename)
        })
      
      
      output$download <- downloadHandler(
        filename = function() { 
          sprintf("summary.xlsx")
        },
        content = function(filename) {
          req(nrow(data_s())>0)
          wb = createWorkbook()
          
          for(i in c("period","available_years","min_year","max_year","nb_year","nb_record")){
          df<-data_s()
          colnames(df)[colnames(df)==config$dcf$reporting_entities$name] = "reporting_entity"
          df<-df[,c("reporting_entity","task",i)]
          
          names(df)[names(df) == i] <- "stat"
          df<-unique(df)
          
          entity_list <- NULL
          if(isTRUE(input$limit_entities_s)){
            entity_list<-unique(df$reporting_entity)
          }else{
            entity_list<-reporting_entities
          }
          if("submitter" %in% profile$shiny_app_roles &
             !any(c("admin", "manager", "superviser") %in% profile$shiny_app_roles)){
            entity_list = entity_list[entity_list %in% profile$reporting_entities]
          }
          
          df<-df%>%
            rowwise()%>%
            mutate(value=ifelse(input$stat_s%in%c("nb_year","nb_record"),as.numeric(stat),1))%>%
            ungroup()%>%
            complete(nesting(task),reporting_entity=entity_list,fill = list(value=0,stat="(no data)"))%>%
            arrange(desc(reporting_entity))
          
          df_values<-df$value
          
          df<-df%>%
            select(-value)%>%
            pivot_wider(names_from = task,values_from = stat,names_sort=T)%>%
            filter(reporting_entity %in% entity_list)%>%
            rename(` `=reporting_entity)
        
          addWorksheet(wb, i)
          setColWidths(wb, i, cols = 1:ncol(df), widths = "auto")
          nodataStyle <- createStyle(fontColour = "black", bgFill = "gray")
          dataStyle <- createStyle(fontColour = "black", bgFill = "#45AD15")
          conditionalFormatting(wb, i, cols = 2:ncol(df), rows = 1:nrow(df)+1, type = "contains", rule = "(no data)", style = nodataStyle)
          conditionalFormatting(wb, i, cols = 2:ncol(df), rows = 1:nrow(df)+1, type = "notcontains", rule = "(no data)", style = dataStyle)
          writeData(wb, sheet = i, x = df, startCol = 1)

          }
          saveWorkbook(wb, filename, overwrite = TRUE)
          
        })
      
      observeEvent(input$task,{
        req(input$task)
        if(!is.null(input$task)|input$task!=""){
          
          # items <- SH$listWSItems(parentFolderID = subset(task_folders,name==input$task)$id)
          # last_modification_time <- max(items$lastModificationTime)
          # items <- items[items$lastModificationTime == last_modification_time,]
          # items <- items [endsWith(items$name,"_db_new.csv"),]
          # item<-SH$downloadItem(item = items, wd = tempdir())
          # file<-readr::read_csv(item)
          #file<-getDataTaskDBData(pool, input$task)
          file<-data_tasks[[input$task]]
          data<-data(file)
        }
      })
      
      #heatmap by task
      output$heatmap<-renderPlotly({
        req(!is.null(data()))
        req(!is.null(input$limit_entities))
        print("RUN HEATMAP")
        df<-data()
        colnames(df)[colnames(df)==config$dcf$reporting_entities$name] <- "reporting_entity"
        df<-subset(df,select=c(reporting_entity,time_end))
        df$time_end<-year(df$time_end)
        df<-unique(df)
        df$value<-1
        
        entity_list <- NULL
        if(isTRUE(input$limit_entities)){
          entity_list<-unique(df$reporting_entity)
        }else{
          entity_list<-reporting_entities
        }
        if("submitter" %in% profile$shiny_app_roles &
           !any(c("admin", "manager", "superviser") %in% profile$shiny_app_roles)){
          entity_list = entity_list[entity_list %in% profile$reporting_entities]
        }
        
        df<-df%>%
          complete(nesting(time_end=full_seq(time_end, 1)),reporting_entity=entity_list,fill = list(value=0))%>%
          rename(year=time_end)%>%
          arrange(desc(reporting_entity),year)%>%
          filter(reporting_entity %in% entity_list)%>%
          pivot_wider(names_from = year,values_from = value,names_sort=T)
        
        print(head(as.data.frame(df)))
        
        y_lab<-df$reporting_entity
        x_lab<-colnames(df)[-1]
        
        df_matrix<-as.matrix(df[,-1])
        
        colorScale <- data.frame(
          z = c(0,1),
          col=c("grey","green")
        ) 
        
        color_s <- setNames(data.frame(c(0,1),c("grey","green") ), NULL)
        
        fig<-plot_ly(
          height = 150+40*length(y_lab),
          x=x_lab,
          y=y_lab,
          z = df_matrix,
          zmin=0,
          zmax=1,
          xgap=10,
          ygap=10,
          colorscale = color_s,
          colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("missing","validate"),len=0.2), type = "heatmap"
        )
        
        fig<-layout(fig,
                    showlegend = FALSE,
                    xaxis = list(tickvals=seq(min(x_lab),max(x_lab),1),ticktext=as.character(seq(min(x_lab),max(x_lab),1)),showgrid = F)
        )
        return(fig)
      })
      
      #heatmap summary
      output$heatmap_s<-renderPlotly({
        req(!is.null(data_s()))
        req(!is.null(input$limit_entities_s))
        req(dataAvailable()|(!dataAvailable()&!input$limit_entities_s))
        print("RUN HEATMAP")
        req(!is.null(input$stat_s))
        print(input$stat_s)
        df<-data_s()
        colnames(df)[colnames(df)==config$dcf$reporting_entities$name] = "reporting_entity"
        df<-df[,c("reporting_entity","task",input$stat_s)]

        names(df)[names(df) == input$stat_s] <- "stat"
        df<-unique(df)
        
        df<-df%>%
        rowwise()%>%
        mutate(value=ifelse(input$stat_s%in%c("nb_year","nb_record"),as.numeric(stat),1))%>%
        ungroup()
        
        max_value<-max(df$value,na.rm=T)
        
        print(max_value)
        entity_list <- NULL
        if(isTRUE(input$limit_entities_s)){
          entity_list<-unique(df$reporting_entity)
        }else{
          entity_list<-reporting_entities
        }
        if("submitter" %in% profile$shiny_app_roles &
           !any(c("admin", "manager", "superviser") %in% profile$shiny_app_roles)){ 
          entity_list = entity_list[entity_list %in% profile$reporting_entities]
        }
        
        df<-df%>%
          complete(task = names(getTasks(config,withId=TRUE)),reporting_entity=entity_list,fill = list(value=0,stat="(no data)"))%>%
          arrange(desc(reporting_entity))%>%
          filter(reporting_entity %in% entity_list)
        
        text<-df%>%
          select(-value)
        
        dfm<-df%>%
          select(-stat)%>%
          pivot_wider(names_from = task,values_from = value,names_sort=T)
        

          
        
        print(head(as.data.frame(dfm)))
        print(head(as.data.frame(text)))
        
        y_lab<-dfm$reporting_entity
        x_lab<-colnames(dfm)[-1]
        
        df_matrix<-as.matrix(dfm[,-1])
        
        # colorScale <- data.frame(
        #   z = c(0,1),
        #   col=c("grey","green")
        # ) 
        
        if(input$stat_s%in%c("nb_year","nb_record")){
        print("HERE")
        fig<-plot_ly(
          height = 150+40*length(y_lab),
          x=x_lab,
          y=y_lab,
          z = df_matrix,
          zmin=0,
          zmax=max(as.numeric(text$stat)),
          xgap=10,
          ygap=10,
          color = df$value,
          colors = c("grey", "#45AD15"),
          type = "heatmap"
        )
        
        }else{
          
        fig<-plot_ly(
          height = 150+40*length(y_lab),
          x=x_lab,
          y=y_lab,
          z = df_matrix,
          zmin=0,
          zmax=1,
          xgap=10,
          ygap=10,
          colorscale = setNames(data.frame(c(0,1),c("grey","#45AD15") ), NULL),
          colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("no data","data"),len=0.2), 
          type = "heatmap"
        )
        
        }
        
        fig<-layout(fig,
                    showlegend = FALSE,
                    xaxis = list(side ="top",showgrid = F)
        )%>% add_annotations(x = text$task, y = text$reporting_entity, text = text$stat, xref = 'x', yref = 'y', showarrow = FALSE, font=list(color='black'))
        return(fig)
      })
      
      #nodata wrapper for bytask view
      output$nodata<-renderUI({
        req(is.null(data()))
        req(!is.null(input$task))
        req(input$task!="")
       p("(No data available)")
      })
      
      output$heatmap_wrapper<-renderUI({
        req(!is.null(input$task))
        req(input$task!="")
        if(is.null(data())){
          uiOutput(ns("nodata"))
        }else{
          fluidRow(
            withSpinner(plotlyOutput(ns("heatmap")),type=4)
          )
        }
      })
      
       #nodata wrapper for summary
      output$nodata_s<-renderUI({
        req(!is.null(data_s()))
        req(!is.null(input$limit_entities_s))
        req(!dataAvailable()&input$limit_entities_s)
       p("(No data available)")
      })
      
      output$heatmap_s_wrapper<-renderUI({
        req(!is.null(dataAvailable()))
        req(!is.null(input$limit_entities_s))
        if(!dataAvailable()&input$limit_entities_s){
          uiOutput(ns("nodata_s"))
        }else{
          fluidRow(
            withSpinner(plotlyOutput(ns("heatmap_s")),type=4)
          )
        }
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}
