function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      SH <- components$STORAGEHUB
      task_folders <- SH$listWSItems(config$dataspace_id)
      reporting_entities<-config$dcf$reporting_entities$codelist_ref$code
      
      data<-reactiveVal(NULL)
      
      #status selector
      output$entities_selector <- renderUI({
        checkboxInput(ns("limit_entities"), "Limit to entities with data", TRUE)
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
      
      observeEvent(input$task,{
        req(input$task)
        if(!is.null(input$task)|input$task!=""){
          
          items <- SH$listWSItems(parentFolderID = subset(task_folders,name==input$task)$id)
          last_modification_time <- max(items$lastModificationTime)
          items <- items[items$lastModificationTime == last_modification_time,]
          items <- items [endsWith(items$name,"_db_new.csv"),]
          item<-SH$downloadItem(item = items, wd = tempdir())
          file<-readr::read_csv(item)
          data<-data(file)
        }
      })
      
      output$heatmap<-renderPlotly({
        req(!is.null(data()))
        req(!is.null(input$limit_entities))
        print("RUN HEATMAP")
        df<-subset(data(),select=c(flagstate,time_end))
        df$time_end<-year(df$time_end)
        df<-unique(df)
        df$value<-1
        
        if(isTRUE(input$limit_entities)){
          entity_list<-unique(df$flagstate)
        }else{
          entity_list<-reporting_entities
        }
        
        df<-df%>%
          complete(nesting(time_end),flagstate=entity_list,fill = list(value=0))%>%
          rename(year=time_end)%>%
          arrange(desc(flagstate),year)%>%
          pivot_wider(names_from = year,values_from = value,names_sort=T)
        
        y_lab<-df$flagstate
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
      
      #-----------------------------------------------------------------------------------
    }
  )
}