

#getTasks
getTasks <- function(config,withId=FALSE){
  tasks <- config$dcf$tasks
  task_list <- names(tasks)
  if(withId){
    task_list<- setNames(task_list, sprintf("%s [%s]",unlist(lapply(tasks, function(x){x$name})),task_list))
  }else{
    task_list<- setNames(task_list, unlist(lapply(tasks, function(x){x$name})))
  }
  return(task_list)
}

#getTaskProfile
getTaskProfile <- function(config,id){
  task <- config$dcf$tasks[[id]]
  return(task)
}

#eval_variable_expression
eval_variable_expression <- function(str){
  eval_str <- eval(parse(text = whisker::whisker.render(str, as.list(Sys.getenv()))))
  return(eval_str)
}
#data_time
data_time <- function(start_year, end_year){
  as.Date(sprintf("%s-01-01/%s-12-31", start_year, end_year))
}
#data_time_start
data_time_start <- function(year){
  as.Date(sprintf("%s-01-01", year))
}
#data_time_end
data_time_end <- function(year){
  as.Date(sprintf("%s-12-31", year))
}

#qtodate
qtodate<- function(date,period="start"){
  
  date<-unlist(strsplit(date,"-") )
  
  Y<-date[1]
  Q<-date[2]
  
  Q<-switch (Q,
             Q1=c('01-01','03-31'),
             Q2=c('04-01','06-30'),
             Q3=c('07-01','09-30'),
             Q4=c('10-01','12-31')
  )
  
  date<-paste0(Y,"-",Q)
  
  if(period=="start")return(date[1])
  if(period=="end")return(date[2])
  if(period=="start+end")return(paste0(date[1],"/",date[2]))
  
}

#ymtodate
ymtodate<- function(date,period="start"){
  start_date<-ymd(paste0(date,"-01"))
  end_date<-ceiling_date(start_date,unit= "month") -1
  if(period=="start")return(as.character(start_date))
  if(period=="end")return(as.character(end_date))
  if(period=="start+end")return(paste0(as.character(start_date),"/",as.character(end_date)))
}

#ytodate
ytodate<- function(date,period="start"){
  Y<-unlist(strsplit(date,"-"))[1]
  start_date<-paste0(Y,"-01-01")
  end_date<-paste0(Y,"-12-31")
  if(period=="start")return(start_date)
  if(period=="end")return(end_date)
  if(period=="start+end")return(paste0(start_date,"/",end_date))
}

#dateFormating
dateFormating<- function(date,period="start"){
  dates<-c()
  for(i in c(1:length(date))){
    if(startsWith(unlist(strsplit(date[i],"-"))[2],"Q")){
      x<-qtodate(date[i],period=period)
    }else if(startsWith(unlist(strsplit(date[i],"-"))[2],"M")){
      x<-ymtodate(gsub("M","",date[i]), period=period)
    }else if(unlist(strsplit(date[i],"-"))[2]=="NA"){
      x<-ytodate(date[i],period=period)
    }else{
      x<-ymtodate(date[i],period=period)
    }
    dates<-c(dates,x)
  }
  return(dates)
}

#readTaskDefinition
#@note new, refactoring with vrule
readTaskDefinition <- function(file){
  task_def <-jsonlite::read_json(file)
  
  #mandatory properties
  mandatory_properties = c("task_id", "task_name", "formats")
  diff = setdiff(mandatory_properties, names(task_def))
  if(length(diff)>0){
    stop(sprintf("Task definition | missing mandatory properties: %s", paste0(diff, collapse=",")))
  }
  
  #conditional properties
  if("simplified" %in% names(task_def$formats)){
    #in case a simplified format definition is set, the measurement is mandatory
    if(!"measurement" %in% names(task_def)){
      stop("Task definition |  At least one 'measurement' declaration is required with a simplified format specification")
    }
  }
  
  #formats
  mandatory_format_properties = c("id", "name", "ref")
  for(format in names(task_def$formats)){
    diff = setdiff(mandatory_format_properties, names(task_def$formats[[format]]))
    if(length(diff)>0){
      stop(sprintf("Task definition | At least one format with missing properties: %s", paste(diff, collapse=",")))
    }
    task_def$formats[[format]]$spec <- vrule::format_spec$new(json = jsonlite::read_json(task_def$formats[[format]]$ref))
  }
  return(task_def)
}

#readDataCallRules
readDataCallRules<- function(task_def, format, config = NULL,reporting_entity=NULL,data_call_limited_on=NULL){

  #inherit vrule format_spec object from task definition
  format_spec = task_def$formats[[format]]$spec 
  
  #we create a deep clone (copy) of the format_spec
  dc_format_spec = format_spec$clone(deep = TRUE)
  dc_format_spec$column_specs = list()
  
  #REPORTING ENTITY data call rule
  if(!is.null(config$dcf$reporting_entities)){
    #we alter the vrule associated to the reporting entity target column 
    #to limit it to the reporting entity selected by the data submitter
    reporting_entity_col_spec <- format_spec$getColumnSpecByName(config$dcf$reporting_entities$name)$clone(deep = TRUE)
    entity_col_spec = vrule::column_spec$new()
    entity_col_spec$setName(reporting_entity_col_spec$name)
    entity_col_spec$rules = list()
    cl_rule = vrule::vrule_raw_codelist$new(ref_values = list(reporting_entity))
    entity_col_spec$addRule(cl_rule)
    dc_format_spec$addColumnSpec(entity_col_spec)
  }
  
  #TEMPORAL EXTENT data call limitations
  if(!is.null(data_call_limited_on)){
    
    #TIME DATA CALL CONSISTANCY
    if("time_start" %in% names(data_call_limited_on)){
      threshold <- eval_variable_expression(data_call_limited_on[["time_start"]])
      time_start_col_spec <- format_spec$getColumnSpecByName("time_start") #in case time_start is part of format_spec (ie if format is generic)
      if(is.null(time_start_col_spec)){
        time_start_col_spec <- vrule::column_spec$new()
        time_start_col_spec$setName("time_start")
      }
      time_start_col_spec$rules = list()
      time_start_col_spec$addRule(vrule::vrule_date_min$new(minValue = threshold))
      dc_format_spec$addColumnSpec(time_start_col_spec)
    }
  
    if("time_end" %in% names(data_call_limited_on)){
      threshold <- eval_variable_expression(data_call_limited_on[["time_end"]])
      time_end_col_spec <- format_spec$getColumnSpecByName("time_end") #in case time_end is part of format_spec (ie if format is generic)
      if(is.null(time_end_col_spec)){
        time_end_col_spec <- vrule::column_spec$new()
        time_end_col_spec$setName("time_end")
      }
      time_end_col_spec$rules = list()
      time_end_col_spec$addRule(vrule::vrule_date_max$new(maxValue = threshold))
      dc_format_spec$addColumnSpec(time_end_col_spec)
    }
  }
  
  #we copy the task_def and associate the generic format spec tailored to the data call specific validation
  dc_task_def = task_def
  dc_task_def$formats = list(
    generic = list(
      id = "dc",
      name = "data call consistancy format",
      spec = dc_format_spec
    )
  )
  
  return(dc_task_def)
  
}

#getTaskFormats
getTaskFormats<- function(config,id){
  task<-getTaskProfile(config,id)
  taskRules <- task$dsd_ref_url
  task_def<-jsonlite::read_json(taskRules)
  task_def <- task_def$formats
  formats<-setNames(unlist(lapply(task_def, function(x){x$id})),unlist(lapply(task_def, function(x){x$name})))
  return(formats)
}

#readDataFile
readDataFile <- function(file){
  #@eblondel this should be done elsewhere I guess
  data = switch(mime::guess_type(file),
                "application/vnd.ms-excel" = readxl::read_xls(file, col_types = "text"),
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = readxl::read_xlsx(file, col_types = "text"),
                "text/csv" = readr::read_csv(file,col_types = readr::cols(.default = "c")),
                NULL
  )
  if(is.null(data)){
    stop(sprintf("No data reader supported in dcf-shiny for mime type '%s'", mime::guess_type(file)))
  }
  data = as.data.frame(data)
  return(data)
}

#validateDataStructure
validateDataStructure <- function(file, task_def, format){
  
  #inherit vrule format_spec object from task definition
  format_spec = task_def$formats[[format]]$spec 
  format_spec_cols = sapply(format_spec$column_specs, function(x){x$name})
  
  #read data in case it's not already done
  data <- file
  if(!is.data.frame(file)) data <- readDataFile(file)
  
  #structural validation (delegated to vrule package)
  errors <- format_spec$validateStructure(data)
  
  report<-errors[,c("col","type")]
  columns_info<-do.call("rbind",lapply(format_spec$column_specs, function(x){data.frame(name=x$name,required=x$required)}))
  
  summary<-merge(columns_info,report,by.x="name",by.y="col",all.x=T,all.y=F,sort=F)
  
  summary<-summary%>%
    rowwise()%>%
    mutate(required=ifelse(required==TRUE,"MANDATORY","OPTIONAL"))%>%
    mutate(type=ifelse(is.na(type),"EXISTING","MISSING"))%>%
    mutate(name=factor(name,levels=format_spec_cols))%>%
    ungroup()%>%
    arrange(name)%>%
    mutate(name=as.character(name))
  
  names(summary)<-c("column","type","status")
  
  
  if(nrow(subset(errors,type=="ERROR"))>0){
    valid<-FALSE
  }else{
    valid<-TRUE
  }
  
  out<-list(
    errors=errors,
    summary=summary,
    valid=valid
  )
}

#validateDataContent
validateDataContent<-function(file, task_def, format, parallel = FALSE, prettify = FALSE){
  
  #inherit vrule format_spec object from task definition
  format_spec = task_def$formats[[format]]$spec 
  format_spec_cols = sapply(format_spec$column_specs, function(x){x$name})
    
  #read data in case it's not already done
  data <- file
  if(is.tibble(data)) data <- as.data.frame(data)
  if(!is.data.frame(file)) data <- readDataFile(file)

  #prettify for display as handsontable
  if(prettify){
    errors<-format_spec$validate_and_display_as_handsontable(data, parallel = parallel)
  }else{
    errors<-format_spec$validateContent(data, parallel = parallel)
  }
  
  errors<-subset(errors,category!="Data structure")
  
  report<-unique(errors[,c("col","type")])
  columns_info<-do.call("rbind",lapply(format_spec$column_specs, function(x){data.frame(name=x$name)}))
  if(nrow(report>0)){
    summary<-merge(columns_info,report,by.x="name",by.y="col",all.x=T,all.y=F,sort=F)
  }else{
    summary<-columns_info
    summary$type<-NA
  }
  summary<-summary%>%
    rowwise()%>%
    mutate(type=ifelse(is.na(type),"SUCCESS",type))%>%
    group_by(name)%>%
    summarise(status=ifelse(any("ERROR"%in%type),"FAILED",
                      ifelse(any("WARNING"%in%type),"PASSED WITH WARNING","PASSED")))%>%
    ungroup()%>%
    mutate(name=factor(name,levels=format_spec_cols))%>%
    arrange(name)%>%
    mutate(name=as.character(name))

  names(summary)<-c("column","status")
  
  if(nrow(subset(errors,type=="ERROR"))>0){
    valid<-FALSE
  }else{
    valid<-TRUE
  }
  out = list(
    errors = errors,
    summary = summary, 
    valid = valid
  )
  
  return(out)
}

#validateCallRules
#@deprecated
validateCallRules <- function(file, rules, hostess=NULL){
  
  errors<-data.frame(
    type=character(),
    row=character(),
    column=character(),
    category=character(),
    message=character()
  )
  
  tests<-data.frame(
    code=c("SE01","SE02","SW01","SE03"),
    name=c("Consistancy with data call period",
           "Presence of mandatory years",
           "Presence of historical serie",
           "Consistancy with reporting entities"),
    status=c("NOT TESTED","NOT TESTED","NOT TESTED","NOT TESTED"),
    icon=rep(paste0(tags$span(shiny::icon("ban"), title = "Not tested", style = "color:grey;"), collapse=""),4)
  )
  
  #read data in case it's not already done
  data <- file
  if(!is.data.frame(file)) data <- readDataFile(file)
  
  if(!is.null(hostess)) hostess$set(5)
  #TIME DATA CALL CONSISTANCY
  if("time_start" %in% names(rules)){
    rule<-eval_variable_expression(rules[["time_start"]])
    #FIRST CHECK : NO YEAR IN DATA BEFORE DATACALL REQUEST
    data_date<-as.Date(unique(data$time_start))
    cond<-data_date[data_date<rule]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time_start",category="date before allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's start date '%s'",date,rule)))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
  }
  if(!is.null(hostess)) hostess$set(15)
  if("time_end" %in% names(rules)){
    rule<-eval_variable_expression(rules[["time_end"]])
    #FIRST CHECK : NO YEAR IN DATA AFTER DATACALL REQUEST
    data_date<-as.Date(unique(data$time_end))
    cond<-data_date[data_date>rule]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time_end",category="date after allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's end date '%s'",date,rule)))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    #SECOND CHECK : LAST YEAR REQUESTED MUST BE PRESENT
    cond<-year(max(data_date))==year(rule)
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="SE02",row="-",column="time_end",category="missing last year",message=sprintf("Last year requested by the data call is missing of data",year(rule))))
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"FAILED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"PASSED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
  }
  if(!is.null(hostess)) hostess$set(30)
  if(all(c("time_start","time_end") %in% names(rules))){
    rule<-c(eval_variable_expression(rules[["time_start"]]),eval_variable_expression(rules[["time_end"]]))
    #THIRD CHECK : ALL YEAR PRESENT
    if(year(rule[1])!=year(rule[2])){
      req_y<-seq(year(rule[1]),year(rule[2]))
      data_y<-seq(year(min(as.Date(unique(data$time_start)))),year(max(as.Date(unique(data$time_end)))))
      cond<-req_y[!req_y%in%data_y]
      if(length(cond)>0){
        for(year in cond){
          errors<-rbind(errors,data.frame(type="WARNING",rule="SW01",row="-",column="time_start",category="missing historical year",message=sprintf("Historical year '%s' is missing of data call",year)))
          errors<-rbind(errors,data.frame(type="WARNING",rule="SW01",row="-",column="time_end",category="missing historical year",message=sprintf("Historical year '%s' is missing of data call",year)))
        }
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"WARNING"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("exclamation-triangle"), title = "Warning", style = "color:orange;"), collapse="")
        }
      }else{
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"PASSED"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
        }
      }
    }
  }
  if(!is.null(hostess)) hostess$set(45)
  if("time" %in% names(rules)){
    rule<-eval_variable_expression(rules[["time"]])
    #FIRST CHECK : ALL YEAR IN DATA MUST BE INCLUDED IN DATACALL REQUEST
    decode_date<-unlist(strsplit(unique(data$time),"/"))
    time_start<-as.Date(decode_date[1])
    time_end<-as.Date(decode_date[2])
    
    cond<-data_date[time_start<rule[1]]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time",category="date before allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's start date '%s'",date,rule[1])))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    
    cond<-data_date[time_start>rule[2]]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time",category="date after allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's end date '%s'",date,rule[2])))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    
    #SECOND CHECK : LAST YEAR REQUESTED MUST BE PRESENT
    cond<-year(max(time_end))==year(rule[2])
    
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="SE02",row="-",column="time",category="missing last year",message=sprintf("Last year requested by the data call is missing of data",year(rule[2]))))
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"FAILED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"PASSED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    
    #THIRD CHECK : ALL YEAR PRESENT
    if(year(rule[1])!=year(rule[2])){
      req_y<-seq(year(rule[1]),year(rule[2]))
      data_y<-seq(year(min(time_start)),year(max(time_end)))
      cond<-req_y[!req_y%in%data_y]
      if(length(cond)>0){
        for(year in cond){
          errors<-rbind(errors,data.frame(type="WARNING",rule="SW01",row="-",column="time",category="missing historical year",message=sprintf("Historical year '%s' is missing of data call",year)))
        }
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"WARNING"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("exclamation-triangle"), title = "Warning", style = "color:orange;"), collapse="")
        }
      }else{
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"PASSED"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
        }
      }
    }
  }
  if(!is.null(hostess)) hostess$set(60)
  if("reporting_entity" %in% names(rules)){
    rule<-rules[["reporting_entity"]]
    #FIRST CHECK :ONLY ON REPORTING ENTITY IN DATA
    data_reporting_entities<-unique(data$flagstate)
    cond<-length(data_reporting_entities)
    if(cond>1){
      errors<-rbind(errors,data.frame(type="ERROR",rule="SE03",row="-",column="flagstate",category="unique reporting entity",message=sprintf("%s reporting entities are detected in the dataset (%s) and only '%s' should be include",cond,paste0(data_reporting_entities,collapse=","),rule)))
      if(tests[tests$code=="SE03",]$status!="FAILED"){
        tests[tests$code=="SE03",]$status<-"FAILED"
        tests[tests$code=="SE03",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      #SECOND CHECK :REPORTING ENTITY CORRESPONDING TO CORRECT REPORTING ENTITY
      cond<-data_reporting_entities==rule
      if(!cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE03",row="-",column="flagstate",category="conform reporting entity",message=sprintf("reporting entity '%s' is not corresponding to the selected reporting entity '%s'",data_reporting_entities,rule)))
        if(tests[tests$code=="SE03",]$status!="FAILED"){
          tests[tests$code=="SE03",]$status<-"FAILED"
          tests[tests$code=="SE03",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
        }
      }else{
        if(tests[tests$code=="SE03",]$status!="FAILED"){
          tests[tests$code=="SE03",]$status<-"PASSED"
          tests[tests$code=="SE03",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
        }
      }
    }
  }
  if(!is.null(hostess)) hostess$set(75)
  if(nrow(subset(errors,type=="ERROR"))>0){
    valid<-FALSE
  }else{
    valid<-TRUE
  }
  out = list(
    errors = errors,
    tests = tests, 
    valid = valid
  )
  if(!is.null(hostess)) hostess$set(80)
  return(out)
  
}

#completeWithMissingEntities
#@deprecated
completeWithMissingEntities<-function(config,pool,profile,data,user_only=FALSE){
  if(user_only){
    reporting_entities<-data.frame(code=getDBUserReportingEntities(pool,profile))
  }else{
    reporting_entities<-config$dcf$reporting_entities$codelist_ref 
  }
  
  if(nrow(data)>0){
    missing_entities<-subset(reporting_entities,!code%in%data$reporting_entity)$code
    if(length(missing_entities>0)){

      missing <- data.frame(
        id = rep("",length(missing_entities)),
        data_call_id = rep(data$data_call_id[1],length(missing_entities)),
        data_call_folder = rep("",length(missing_entities)),
        task_id = rep(data$task_id[1],length(missing_entities)),
        reporting_entity = missing_entities,
        temporal_extent = rep("",length(missing_entities)),
        submitter = rep("",length(missing_entities)),
        creationTime = rep(NA,length(missing_entities)),
        lastModifiedBy = rep("",length(missing_entities)),
        lastModificationTime = rep(NA,length(missing_entities)),
        status = rep("MISSING",length(missing_entities)),
        stringsAsFactors = FALSE)
      
      data<-rbind(data,missing)
    }
  }else{
    data <- data.frame(
      id = rep("",nrow(reporting_entities)),
      data_call_id = rep("",nrow(reporting_entities)),
      data_call_folder = rep("",nrow(reporting_entities)),
      task_id = rep("",nrow(reporting_entities)),
      reporting_entity = reporting_entities$code,
      temporal_extent = rep("",nrow(reporting_entities)),
      submitter = rep("",nrow(reporting_entities)),
      creationTime = rep(NA,nrow(reporting_entities)),
      lastModifiedBy = rep("",nrow(reporting_entities)),
      lastModificationTime = rep(NA,nrow(reporting_entities)),
      status = rep("MISSING",nrow(reporting_entities)),
      stringsAsFactors = FALSE)
  }
  return(data)
}
