

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

#getTaskProperties
getTaskProperties <- function(config,id){
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
  start_date<-paste0(date,"-01")
  end_date<-as.character(ceiling_date(ymd(start_date),unit= "month") -1)
  if(period=="start")return(start_date)
  if(period=="end")return(end_date)
  if(period=="start+end")return(paste0(start_date,"/",end_date))
  
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
    }else if(unlist(strsplit(date[i],"-"))[2]=="NA"){
      x<-ytodate(date[i],period=period)
    }else{
      x<-ymtodate(date[i],period=period)
    }
    dates<-c(dates,x)
  }
  return(dates)
}

# #readTaskColumnDefinitions
# readTaskColumnDefinitions<- function(file, format, config = NULL, reporting_entity = NULL,force=F){
#   task_def<-jsonlite::read_json(file)
#   task_def <- task_def$formats[[format]]$columns
#   if(!is.null(config$dcf$reporting_entities)) if(config$dcf$reporting_entities$validation|force){
#     if(!is.null(task_def[[config$dcf$reporting_entities$name]]$ref)) task_def[[config$dcf$reporting_entities$name]]$ref <- NULL
#     task_def[[config$dcf$reporting_entities$name]]$allowed_values <- reporting_entity
#     task_def[[config$dcf$reporting_entities$name]]$rule_type <- "reporting_entity"
#   }
#   return(task_def)
# }

#readTaskColumnDefinitions
readTaskColumnDefinitions<- function(file, format, config = NULL){
  
  task_def<-jsonlite::read_json(file)
  
  task_def <- task_def$formats[[format]]$columns
  
  if(is.null(task_def$column_specs)){
    if(!is.null(task_def$ref)){
      task_def<-jsonlite::read_json(task_def$ref)
    }else{
      stop()
    }
  }
  
  # if(!is.null(config$dcf$reporting_entities)) if(limit_reporting_entities){
  #   
  #   reporting_entity_col_spec_idx<-which(sapply(task_def$column_specs, function(x) any(which(x$name == config$dcf$reporting_entities$name))))
  #   
  #   target_rule_idx<-which(sapply(task_def$column_specs[[reporting_entity_col_spec_idx]]$rules, function(x) any(which(x$vrule == "codelist"))))
  #   
  #   if(!is.null(target_rule_idx)){
  #     task_def$column_specs[[reporting_entity_col_spec_idx]]$rules[[target_rule_idx]]$args<-NULL
  #     task_def$column_specs[[reporting_entity_col_spec_idx]]$rules[[target_rule_idx]]$args$ref_values<-list(reporting_entity)
  #   }
  #   
  # }
  
  return(task_def)
  
}

#readDataCallRules
readDataCallRules<- function(file, format, config = NULL,reporting_entity=NULL,data_call_limited_on=NULL){
  
  task_def<-jsonlite::read_json(file)
  
  task_def <- task_def$formats[[format]]$columns
  
  if(is.null(task_def$column_specs)){
    if(!is.null(task_def$ref)){
      task_def<-jsonlite::read_json(task_def$ref)
    }else{
      stop()
    }
  }
  
  dc_task_def<-task_def
  
  dc_task_def$column_specs<-NULL
  
  if(!is.null(config$dcf$reporting_entities)){
    
    reporting_entity_col_spec_idx<-which(sapply(task_def$column_specs, function(x) any(which(x$name == config$dcf$reporting_entities$name))))
    reporting_entity_col_spec<-task_def$column_specs[[reporting_entity_col_spec_idx]]
    reporting_entity_col_spec$rules<-NULL
    reporting_entity_col_spec$rules[[1]]<-list(vrule="codelist",
                                       args=list(ref_values=list(reporting_entity)))

    
    dc_task_def$column_specs[[length(dc_task_def$column_specs) + 1]] <- reporting_entity_col_spec
    
  }
  
  if(!is.null(data_call_limited_on)){
    
    #TIME DATA CALL CONSISTANCY
    if("time_start" %in% names(data_call_limited_on)){
      
      threshold<-eval_variable_expression(data_call_limited_on[["time_start"]])
      
      time_start_col_spec_idx<-which(sapply(task_def$column_specs, function(x) any(which(x$name == "time_start"))))
      time_start_col_spec<-task_def$column_specs[[time_start_col_spec_idx]]
      time_start_col_spec$rules<-NULL
      time_start_col_spec$rules[[1]]<-list(vrule="date_min",
                                      args=list(minValue=threshold))
      
      
      dc_task_def$column_specs[[length(dc_task_def$column_specs) + 1]] <- time_start_col_spec

  }
  
    if("time_end" %in% names(data_call_limited_on)){
      
      threshold<-eval_variable_expression(data_call_limited_on[["time_end"]])
      
      time_end_col_spec_idx<-which(sapply(task_def$column_specs, function(x) any(which(x$name == "time_end"))))
      time_end_col_spec<-task_def$column_specs[[time_end_col_spec_idx]]
      time_end_col_spec$rules<-NULL
      time_end_col_spec$rules[[1]]<-list(vrule="date_max",
                                           args=list(maxValue=threshold))
      
      
      dc_task_def$column_specs[[length(dc_task_def$column_specs) + 1]] <- time_end_col_spec

    }
  }
  
  return(dc_task_def)
  
}

#getTaskFormats
getTaskFormats<- function(config,id){
task<-getTaskProperties(config,id)
taskRules <- task$dsd_ref_url
task_def<-jsonlite::read_json(taskRules)
task_def <- task_def$formats
formats<-setNames(unlist(lapply(task_def, function(x){x$id})),unlist(lapply(task_def, function(x){x$name})))
return(formats)
}

validateDataStructure<-function(file,task_def){
  
  error_report=vrule::format_spec$new(json=task_def)
  
  task_cols<-sapply(task_def$column_specs, function(x){x$name})
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{
      print("ERROR")
    }
  }
  
  data<-as.data.frame(data)
  
  errors<-error_report$validateStructure(data)
  
  report<-errors[,c("col","type")]
  columns_info<-do.call("rbind",lapply(task_def$column_specs, function(x){data.frame(name=x$name,required=x$required)}))
  
  summary<-merge(columns_info,report,by.x="name",by.y="col",all.x=T,all.y=F,sort=F)
  
  summary<-summary%>%
    rowwise()%>%
    mutate(required=ifelse(required==TRUE,"MANDATORY","OPTIONAL"))%>%
    mutate(type=ifelse(is.na(type),"EXISTING","MISSING"))%>%
    mutate(name=factor(name,levels=task_cols))%>%
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

validateDataContent<-function(file,task_def,config =NULL,hostess=NULL,prettify=FALSE){
  
    error_report=vrule::format_spec$new(json=task_def)
    
    task_cols<-sapply(task_def$column_specs, function(x){x$name})
    
    if(is.data.frame(file)){
      data<-file
    }else{
      if(any(endsWith(file,c("xls","xlsx")))){
        data<-read_excel(file,col_types = "text")
      }else if(any(endsWith(file,"csv"))){
        data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
      }else{
        print("ERROR")
      }
    }
    
    data<-as.data.frame(data)
    
    if(prettify){
      errors<-error_report$validate_and_display_as_handsontable(data)
    }else{
      errors<-error_report$validateContent(data)
    }
    
    errors<-subset(errors,category!="Data structure")
    
    report<-unique(errors[,c("col","type")])
    columns_info<-do.call("rbind",lapply(task_def$column_specs, function(x){data.frame(name=x$name)}))
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
      mutate(name=factor(name,levels=task_cols))%>%
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

#standardizeNames
standardizeNames<-function(file,task_def,exclude_unused=T){
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  task_cols<-sapply(task_def$column_specs, function(x){x$name})
  data_names<-names(data)
  
  for (i in 1:length(task_def$column_specs)){
    target<-task_def$column_specs[[i]]
    std_name<-target$name
    alt_names<-unlist(target$aliases)
    if(std_name%in%data_names){}else{
      if(any(alt_names%in%data_names)){
        usedName<-alt_names[alt_names%in%data_names]
        names(data)[names(data) == usedName] <- std_name
      }
    }
  }
  
  if(exclude_unused){
    data<-data[intersect(task_cols,names(data))]
  }
  
  return(data)
}

#simplifiedToGeneric
simplifiedToGeneric<-function(file,rules){
  
  rules<-jsonlite::read_json(rules)
  
  task_def <- rules$formats[["generic"]]$columns
  
  if(is.null(task_def$column_specs)){
    if(!is.null(task_def$ref)){
      task_def<-jsonlite::read_json(task_def$ref)
    }else{
      stop()
    }
  }
  
  type=rules$measurement
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  names(data)<-gsub("_unit","__measurement_unit",names(data))
  
  measurement_type<-paste0(type,"_",unlist(task_def$column_specs[[which(sapply(task_def$column_specs, function(x) any(which(x$name == "measurement_type"))))]]$rules[[1]]$args$ref_values))
  
  names(data)[names(data) %in% measurement_type]<-paste0(names(data)[names(data) %in% measurement_type],"__measurement_value")
  
  names(data)[grepl("__measurement",names(data),fixed = T)]<-gsub(paste0(type,"_"),"",names(data)[grepl("__measurement",names(data),fixed = T)])
  
  newdata<-data%>%
    pivot_longer(names(data)[grepl("__measurement",names(data),fixed = T)],names_to = c("measurement_type", ".value"), 
                 names_sep="__" )%>%
    mutate(measurement=type)%>%
    rowwise()%>%
    mutate(date=ifelse("period"%in%names(data),paste0(year,"-",period),paste0(year,"-NA")))%>%
    ungroup()%>%
    mutate(time_start=dateFormating(date,"start"),
           time_end=dateFormating(date,"end"),
           time=paste0(time_start,"/",time_end))
  
  if("measurement_obs"%in%sapply(task_def$column_specs, function(x){x$name})){
    if(any(endsWith(names(newdata),"_obs"))){
      names(newdata)[endsWith(names(newdata),"_obs",names(newdata))] <- "measurement_obs" 
    }else{
      newdata$measurement_obs<-NA
    }
    
  }
  
  newdata<-newdata[sapply(task_def$column_specs, function(x){x$name})]
  
  return(newdata)
}

#validateCallRules
validateCallRules <- function(file, rules,hostess=NULL){
  
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
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data <- readxl::read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
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
completeWithMissingEntities<-function(config,pool,profile,data,user_only=FALSE){
  if(user_only){
    reporting_entities<-data.frame(code=getDBUserReportingEntities(pool,profile))
  }else{
    reporting_entities<-config$dcf$reporting_entities$codelist_ref 
  }
  
  if(nrow(data)>0){
    missing_entities<-subset(reporting_entities,!code%in%data$reporting_entity)$code
    if(length(missing_entities>0)){
      
      print(missing_entities)
      print(length(missing_entities))
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
