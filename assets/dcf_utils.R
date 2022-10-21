#read_dcf_config
read_dcf_config <- function(file){
  
  cfg <- suppressWarnings(yaml::read_yaml(config_file))
  
  #language
  if(is.null(cfg$language)) cfg$language <- "en"
  
  #dcf framework
  if(is.null(cfg$dcf)){
    stop("No 'dcf' frame in configuration")
  }else{
    if(is.null(cfg$dcf$name)) stop("No dcf 'name' defined in configuration")
    if(is.null(cfg$dcf$context)) stop("No dcf 'context' defined in configuration")
    if(is.null(cfg$dcf$workspace)) stop ("No dcf 'workspace' defined in configuration")
    if(is.null(cfg$dcf$roles)) stop("No dcf 'roles' defined in configuration")
    if(is.null(cfg$dcf$roles$submitter)) stop("No dcf roles 'submitter' name defined in configuration")
    if(is.null(cfg$dcf$roles$manager)) stop("No dcf roles 'manager' name defined in configuration")
    if(is.null(cfg$dcf$tasks)) stop("No dcf 'tasks' defined in configuration")
  }
  
  #reporting entity
  reporting_entity <- cfg$dcf$reporting_entities
  if(!is.null(reporting_entity)){
    if(is.null(reporting_entity$name)){
      stop("No name for reporting entity")
    }
    if(is.null(reporting_entity$codelist_ref_url)){
      stop(sprintf("No codelist ref URL for user property '%s'", reporting_entity$name))
    }
    cfg$dcf$reporting_entities$codelist_ref <- readr::read_csv(cfg$dcf$reporting_entities$codelist_ref_url)
    if(is.null(cfg$dcf$reporting_entities$validation)) cfg$dcf$reporting_entities$validation <- TRUE
  }
  
  return(cfg)
}

#getReportingEntityCodes
getReportingEntityCodes <- function(config){
  return(config$dcf$reporting_entities$codelist_ref)
}

#getDataCalls
getDataCalls <- function(pool,status=NULL,tasks=NULL,period=NULL,id_data_call=NULL){
  
  data<-DBI::dbReadTable(pool, "dcf_data_call")
  if(!is.null(id_data_call))data<-data[data$id_data_call%in%id_data_call,]
  if(!is.null(status))data<-data[data$status%in%status,]
  if(!is.null(tasks))data<-subset(data,task_id%in%tasks)
  if(!is.null(period)){
    if(period=="IN")data<-subset(data,date_start<=Sys.Date()&date_end>=Sys.Date())
    if(period=="OUT")data<-subset(data,date_start>Sys.Date()|date_end<Sys.Date())
  }
  return(data)
}

#updateDataCallStatus-auto-closure
closeExpiredDataCalls <- function(pool,config,profile){
  conn <- pool::poolCheckout(pool)
  update_date <- Sys.time()
  out_calls <- getDataCalls(pool, status = "OPENED",period="OUT")
  attr(update_date, "tzone") <- "UTC"
  update_sql <- sprintf("UPDATE dcf_data_call 
                               SET status = 'CLOSED', updater_id = '%s', update_date = '%s' 
                               WHERE date_end < '%s' AND status = 'OPENED'", 
                        "system", as(update_date, "character"), as(Sys.time(),"character"))
  out_sql <- try(DBI::dbSendQuery(conn, update_sql))
  updated <- !is(out_sql, "try-error")
  if(!updated){
    attr(updated, "error") <- as(out_sql, "character")
  }else{
    if(nrow(out_calls)>0){
      for(i in 1:nrow(out_calls)){
        out_call<-out_calls[i,]
        recipients <- getDBUsers(pool = pool, profile = profile)
        print(recipients)
        if(nrow(recipients)>0){
          INFO("Sending data call notification to DB users")
          for(i in 1:nrow(recipients)){
            recipient <- recipients[i,]
            INFO("Sending data call notification to '%s'", recipient$username)
            
            sendMessage(
              subject = sprintf("[%s] Data call closed for %s task ID '%s'", config$dcf$name, config$dcf$context, out_call$task_id),
              body = sprintf(
                "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            The data call for the data task ID '%s' is now closed.
            
            In event you not transmitted your data please contact your regional data manager.
            
            Best regards,
            The %s
                         
            ",
                recipient$fullname, 
                config$dcf$name, config$dcf$context, config$dcf$roles$submitter, out_call$task_id,
                config$dcf$roles$manager
              ),
              recipients = as.list(recipient$username),
              profile = profile
            )
          }
        }
      }
    }
  }
  return(updated)
}

#updateDataCallStatus-auto-start
openStartedDataCalls <- function(pool,config,profile){
  conn <- pool::poolCheckout(pool)
  update_date <- Sys.time()
  in_calls <- getDataCalls(pool, status = "CLOSED",period="IN")
  attr(update_date, "tzone") <- "UTC"
  update_sql <- sprintf("UPDATE dcf_data_call 
                               SET status = 'OPENED', updater_id = '%s', update_date = '%s' 
                               WHERE date_end > '%s' AND date_start < '%s' AND status = 'CLOSED'" , 
                        "system", as(update_date, "character"), as(Sys.time(),"character"), as(Sys.time(),"character"))
  in_sql <- try(DBI::dbSendQuery(conn, update_sql))
  updated <- !is(in_sql, "try-error")
  if(!updated){
    attr(updated, "error") <- as(in_sql, "character")
  }else{
    if(nrow(in_calls)>0){
      for(i in 1:nrow(in_calls)){
        in_call<-in_calls[i,]
        recipients <- getDBUsers(pool = pool, profile = profile)
        print(recipients)
         if(nrow(recipients)>0){
          INFO("Sending data call notification to DB users")
          for(i in 1:nrow(recipients)){
            recipient <- recipients[i,]
            INFO("Sending data call notification to '%s'", recipient$username)
            
            sendMessage(
              subject = sprintf("[%s] New Data call open for %s task ID '%s'", config$dcf$name, config$dcf$context, in_call$task_id),
              body = sprintf(
                "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            A new data call has been opened for the data task ID '%s'.
            
            You are kindly invited to validate and submit your data before %s.
            
            Best regards,
            The %s
                         
            ",
                recipient$fullname, 
                config$dcf$name, config$dcf$context, config$dcf$roles$submitter, in_call$task_id,
                as(end,"character"),
                config$dcf$roles$manager
              ),
              recipients = as.list(recipient$username),
              profile = profile
            )
          }
        }
      }
    }
  }
  return(updated)
}

#createDataCall
createDataCall <- function(pool, task = "", start = Sys.Date(), end = Sys.Date(), status = "OPENED",
                           config, profile){
  conn <- pool::poolCheckout(pool)
  idx <- nrow(getDataCalls(pool))+1
  creation_date <- Sys.time()
  attr(creation_date, "tzone") <- "UTC"
  
  #check presence of data calls
  open_calls <- getDataCalls(pool, task = task, status = "OPENED")
  
  if(nrow(open_calls)>0){
    created <- FALSE
    attr(created, "error") <- sprintf("There is already one open data call open for task '%s'", task)
    return(created)
  }
  
  #db management
  insert_sql <- sprintf(
    "INSERT INTO dcf_data_call(id_data_call, task_id, date_start, date_end, status, creator_id, creation_date) 
           VALUES (%s, '%s', '%s', '%s', '%s', '%s', '%s');", 
    idx, task, as(start,"character"), as(end,"character"), status, profile$preferred_username, as(creation_date, "character")
  )
  out_sql <- try(DBI::dbSendQuery(conn, insert_sql))
  created <- !is(out_sql, "try-error")
  if(!created){
    attr(created, "error") <- as(out_sql, "character")
  }else{
    INFO("Data call successfully created for task ID '%s'", task)
    
    recipients <- getDBUsers(pool = pool, profile = profile)
    print(recipients)
    if(status == "OPENED") if(nrow(recipients)>0){
      INFO("Sending data call notification to DB users")
      for(i in 1:nrow(recipients)){
        recipient <- recipients[i,]
        INFO("Sending data call notification to '%s'", recipient$username)
        sendMessage(
          subject = sprintf("[%s] New Data call open for %s task ID '%s'", config$dcf$name, config$dcf$context, task),
          body = sprintf(
            "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            A new data call has been opened for the data task ID '%s'.
            
            You are kindly invited to validate and submit your data before %s.
            
            Best regards,
            The %s
                         
            ",
            recipient$fullname, 
            config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,
            as(end,"character"),
            config$dcf$roles$manager
          ),
          recipients = as.list(recipient$username),
          profile = profile
        )
      }
    }
  }
  return(created)
}

#updateDataCall
updateDataCall <- function(pool, id_data_call, task = "", start = Sys.Date(), end = NULL, status = "OPENED",
                           config,profile){
  conn <- pool::poolCheckout(pool)
  update_date <- Sys.time()
  attr(update_date, "tzone") <- "UTC"
  
  prev_call <- getDataCalls(pool, id_data_call = id_data_call)
  if(prev_call$status=="CLOSED"&status=="OPENED"){
    open_calls <- getDataCalls(pool, tasks = task,status ="OPENED")
    if(nrow(open_calls)>0){
    updated <- FALSE
    attr(updated, "error") <- sprintf("There is already one open data call open for task '%s'", task)
    return(updated)
    }
  }

  #db management
  update_sql <- sprintf("UPDATE dcf_data_call 
                               SET date_start = '%s', date_end = '%s', status = '%s', updater_id = '%s', update_date = '%s' 
                               WHERE id_data_call = %s", 
                        as(start,"character"), ifelse((prev_call$status=="OPENED"&status=="CLOSED"),as(Sys.Date(),"character"),as(end,"character")), status, profile$preferred_username, 
                        as(update_date, "character"), id_data_call)
  out_sql <- try(DBI::dbSendQuery(conn, update_sql))
  updated <- !is(out_sql, "try-error")
  if(!updated){
    attr(updated, "error") <- as(out_sql, "character")
  }else{  
    recipients <- getDBUsers(pool = pool, profile = profile)
    print(recipients)
    if(nrow(recipients)>0){
      INFO("Sending data call notification to DB users")
      for(i in 1:nrow(recipients)){
        recipient <- recipients[i,]
        INFO("Sending data call notification to '%s'", recipient$username)
    
    if(prev_call$status=="OPENED"&status=="CLOSED"){
      sendMessage(
        subject = sprintf("[%s] Data call closed for %s task ID '%s'", config$dcf$name, config$dcf$context, task),
        body = sprintf(
          "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            The data call for the data task ID '%s' is now closed.
            
            In case you did not submit your data, please contact your %s.
            
            Best regards,
            The %s
                         
            ",
          recipient$fullname, 
          config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,
          config$dcf$roles$manager,config$dcf$roles$manager
        ),
        recipients = as.list(recipient$username),
        profile = profile
      )
    }
    if(prev_call$status=="CLOSED"&status=="OPENED"){
      if(start<Sys.Date()){
        sendMessage(
          subject = sprintf("[%s] Data call re-opened for %s task ID '%s'", config$dcf$name, config$dcf$context, task),
          body = sprintf(
            "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            The data call has been re-opened for the data task ID '%s'.
            
            You are kindly invited to validate and submit your data before %s.
            
            Please ignore this email if this action was already done.
            
            Best regards,
            The %s
                         
            ",
            recipient$fullname, 
            config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,
            as(end,"character"),
            config$dcf$roles$manager
          ),
          recipients = as.list(recipient$username),
          profile = profile
        )
      }else{
          sendMessage(
            subject = sprintf("[%s] New Data call open for %s task ID '%s'", config$dcf$name, config$dcf$context, task),
            body = sprintf(
            "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            A new data call has been opened for the data task ID '%s'.
            
            You are kindly invited to validate and submit your data before %s.
            
            Best regards,
            The %s
                         
            ",
                recipient$fullname, 
                config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,
                as(end,"character"),
                config$dcf$roles$manager
              ),
              recipients = as.list(recipient$username),
              profile = profile
            )
          }
        }
    if((prev_call$status=="OPENED"&status=="OPENED") &prev_call$date_end < end){
      
      sendMessage(
        subject = sprintf("[%s] Data call for %s task ID '%s' is extended", config$dcf$name, config$dcf$context, task),
        body = sprintf(
          "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            The data call for the data task ID '%s' was extended to %s (previously %s).
            
            You are kindly invited to validate and submit your data before this date.
            
            Please ignore this email if you have already submitted your data.
            
            Best regards,
            The %s
                         
            ",
          recipient$fullname, 
          config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,
          as(end,"character"),as(prev_call$date_end,"character"),
          config$dcf$roles$manager
        ),
        recipients = as.list(recipient$username),
        profile = profile
      )
      
    }
        
    if((prev_call$status=="OPENED"&status=="OPENED") &prev_call$date_end > end){
          
          sendMessage(
            subject = sprintf("[%s] Data call for %s task ID '%s' is shortened", config$dcf$name, config$dcf$context, task),
            body = sprintf(
              "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            The data call for the data task ID '%s' was shortened to end on %s (previously %s).
            
            You are kindly invited to validate and submit your data before this date.
            
            Please ignore this email if you have already submitted your data.
            
            Best regards,
            The %s
                         
            ",
              recipient$fullname, 
              config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,
              as(end,"character"),as(prev_call$date_end,"character"),
              config$dcf$roles$manager
            ),
            recipients = as.list(recipient$username),
            profile = profile
          )
          
        }
      }
    }
  }
  return(updated)
}

#deleteDataCall (not yet used)
deleteDataCall <- function(pool, id_data_call,config,profile){
  conn <- pool::poolCheckout(pool)
  open_calls <- getDataCalls(pool, id_data_call = id_data_call, status = "OPENED")
  delete_sql <- sprintf("DELETE FROM dcf_data_call WHERE id_data_call = %s", id_data_call)
  out_sql <- try(DBI::dbSendQuery(conn, delete_sql))
  deleted <- !is(out_sql, "try-error")
  
  if(!deleted){
    attr(deleted, "error") <- as(out_sql, "character")
  }else{
    INFO("Data call '%s' successfully deleted", id_data_call)
    
    #check presence of data calls
    if(nrow(open_calls)>0){
      recipients <- getDBUsers(pool = pool, profile = profile)
      if(nrow(recipients)>0){
        INFO("Sending data call notification to DB users")
        for(i in 1:nrow(recipients)){
          recipient <- recipients[i,]
          INFO("Sending data call notification to '%s'", recipient$username)
          sendMessage(
            subject = sprintf("[%s]Cancellation of Data call (%s) open for %s task ID '%s'", config$dcf$name,id_data_call, config$dcf$context, open_calls$task_id),
            body = sprintf(
              "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            The previously opened data call for the data task ID '%s' was canceled.
            
            Best regards,
            The %s
                         
            ",
              recipient$fullname, 
              config$dcf$name, config$dcf$context, config$dcf$roles$submitter, open_calls$task_id,
              config$dcf$roles$manager
            ),
            recipients = as.list(recipient$username),
            profile = profile
          )
        }
      } 
    }
  }
  return(deleted)
}

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
  Y<-unlist(strsplit(date[i],"-"))[1]
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

#readTaskColumnDefinitions
readTaskColumnDefinitions<- function(file, format, config = NULL, reporting_entity = NULL){
  task_def<-jsonlite::read_json(file)
  task_def <- task_def$formats[[format]]$columns
  if(!is.null(config$dcf$reporting_entities)) if(config$dcf$reporting_entities$validation){
    if(!is.null(task_def[[config$dcf$reporting_entities$name]]$ref)) task_def[[config$dcf$reporting_entities$name]]$ref <- NULL
    task_def[[config$dcf$reporting_entities$name]]$allowed_values <- reporting_entity
    task_def[[config$dcf$reporting_entities$name]]$rule_type <- "reporting_entity"
  }
  return(task_def)
}

#validateData
validateData<-function(file, task_def, config = NULL){
  
  rules<-task_def
  
  errors<-data.frame(
    type=character(),
    row=character(),
    column=character(),
    category=character(),
    message=character()
  )
  
  #TODO read a file (json) with possible validity rules
  #TODO filter on validity rules that apply this app context
  tests<-data.frame(
    code=c("E01","E02","E03","E04","E05", "E06", "I01"),
    name=c("Readable Dataset",
           "Structure of Dataset",
           "No missing Values",
           "Conformity with Standards",
           "Valid Reporting entity",
           "Valid Dates",
           "Skipped Information")
  )
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{
      errors<-rbind(errors,data.frame(type="ERROR",rule="E01",row="-",column="-",category="Dataset format not readable",message=sprintf("Dataset can't be read")))
      out = list(
        errors = errors,
        tests= tests,
        valid = FALSE
      )
      
      return(out)
    }
  }
  
  data_names<-names(data)
  generic_cols<-names(unlist(lapply(rules, function(x){x$rule_type[x$rule_type=="generic"]})))
  special_cols<-names(unlist(lapply(rules, function(x){x$rule_type[x$rule_type=="special"]})))
  reporting_entity_col <- NULL
  if(!is.null(config$dcf$reporting_entities)) if(config$dcf$reporting_entities$validation){
    reporting_entity_cols <- rules[sapply(rules, function(x){x$rule_type=="reporting_entity"})]
    reporting_entity_col <- reporting_entity_cols[[1]]
  }
  
  # ERRORS DETECTION
  ##MANDATORIES COLUMNS
  INFO("Check generic columns")
  for (i in generic_cols){
    
    x<-rules[[i]]
    
    ### COLUMNS PRESENCE
    allowed_names<-c(x$id,unlist(x$aliases))
    cond<-any(allowed_names%in%data_names)
    
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column=x$id,category="Missing mandatory column",message=sprintf("Column '%s' is missing",x$id)))
    }else{
      usedName<-allowed_names[allowed_names%in%data_names]
      checkedCol<-data[[usedName]]
      
      ### NA VALUES PRESENCE
      if(!x$na_allowed){
        cond<-any(is.na(checkedCol))
        if(cond){
          cond<-all(is.na(checkedCol))
          if(cond){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
          }else{
            rows<-which(is.na(checkedCol))
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
            }
          }
        }
      }
      
      ### VALUES VALIDITY
      #### VALUES IN REFERENTIALS  
      if(!is.null(x$ref)){
        ref<-readr::read_csv(x$ref)
        cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%ref$code)
        if(!cond){
          rows<-setdiff(which(!checkedCol%in%ref$code),which(is.na(checkedCol)))
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("Value '%s' is not a valid value of '%s' referential",checkedCol[rowid],usedName)))
          }
        }
      }
      # #### VALUE IN ALLOWED VALUES
      if(!is.null(x$allowed_values)){
        ref<-unlist(x$allowed_values)
        cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%ref)
        if(!cond){
          rows<-setdiff(which(!checkedCol%in%ref),which(is.na(checkedCol)))
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("Value '%s' is not a valid value of '%s' referential",checkedCol[rowid],usedName)))
          }
        }
      }
      #### ADDITIONALS RULES
      ##### UNITS VALIDITY
      if(x$id%in%c("catch_unit","measurement_unit")){
        valid_unit<-unique(units::valid_udunits()$symbol)   
        cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%valid_unit)
        if(!cond){
          rows<-setdiff(which(!checkedCol%in%valid_unit),which(is.na(checkedCol)))
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid value of '%s' referential",checkedCol[rowid],usedName)))
          }
        }
      }
      ##### YEAR VALIDITY
      if(x$id=="year"){
        cond<-nchar(checkedCol)!=4|!any(startsWith(as.character(checkedCol),c("1","2")))|checkedCol>as.integer(substr(Sys.Date(),1,4))
        if(any(cond)){
          rows<-which(cond)
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid year",checkedCol[rowid])))
          }
        }
      }
      
    }
    
    data_names<-data_names[!data_names %in% usedName]
  }
  
  #CONDITIONALS COLUMNS
  ## PERID VALIDITY
  INFO("Check special columns (time)")
  if(all(c("time","time_start","time_end")%in% special_cols)){
    
    ## TIME ON COLUMN FORMAT  
    date_cols<-list()
    for (i in c("time","time_start","time_end")){
      x<-rules[[i]]
      type<-ifelse(i=="time","combined","separated")
      allowed_names<-c(x$id,unlist(x$aliases))
      cond<-any(allowed_names%in%data_names)
      if(cond){
        usedName<-allowed_names[allowed_names%in%data_names]
        checkedCol<-data[[usedName]]
        cond<-any(is.na(checkedCol))
        if(!cond){
          date_cols[[type]][i]<-list(checkedCol)
        }else{
          date_cols[[type]][i]<-list(NULL)
        }
      }else{
        date_cols[[type]][i]<-list(NULL)
      }
    }
    
    if(is.null(date_cols$combined$time)&is.null(date_cols$separated$time_start)&is.null(date_cols$separated$time_end)){
      errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column="time",category="Missing mandatory column",message="'time' or 'time_start' and 'time_end' columns are missing"))
    }else{
      if(!is.null(date_cols$combined$time)){
        #COMBINED TIME
        
        checkedCol<-date_cols$combined$time
        
        x<-rules[["time"]]
        allowed_names<-c(x$id,unlist(x$aliases))
        usedName<-allowed_names[allowed_names%in%data_names]
        ### NA VALUES PRESENCE
        if(!x$na_allowed){
          cond<-any(is.na(checkedCol))
          if(cond){
            cond<-all(is.na(checkedCol))
            if(cond){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
            }else{
              rows<-which(is.na(checkedCol))
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
              }
            }
          }
        }
        
        ### VALUE FORMAT
        decode_date<-strsplit(checkedCol,"/")
        
        cond<-sapply(decode_date,function(x){
          time_start<-x[1]
          valid_start<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_start)
          time_end<-x[2]
          valid_end<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_start)
          if(valid_start&valid_end){
            date_order<-as.Date(time_end)>=as.Date(time_start)
            if(date_order){
              date_impossible<-(as.Date(time_start)>Sys.Date()|as.Date(time_end)>Sys.Date())  
              if(date_impossible){
                return("Date after today")
              }else{
                return("valid")
              }
            }else{
              return("Date end before date start")
            }
          }else{
            return("Invalid date format")
          }
        })
        if(any(cond!="valid")){
          rows<-which(cond!="valid")
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column="time",category="Invalid date",message=cond[rowid]))
          }
        }
        
        data_names<-data_names[!data_names %in% usedName]   
        
      }else{
        #SEPARATED TIME
        if(is.null(date_cols$separated$time_start)|is.null(date_cols$separated$time_end)){
          if(is.null(date_cols$separated$time_start)){
            #TIME START MISSING
            errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column="time_start",category="Missing mandatory column",message="'time_end' is completed but 'time_start' is missing"))
          }else{
            #TIME END MISSING
            errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column="time_end",category="Missing mandatory column",message="'time_start' is completed but 'time_end' is missing"))
          }
        }else{
          #BOTH COLUMN COMPLETED
          
          #TIME START
          time_start<-date_cols$separated$time_start
          x<-rules[["time_start"]]
          allowed_names<-c(x$id,unlist(x$aliases))
          usedName<-allowed_names[allowed_names%in%data_names]
          ### NA VALUES PRESENCE
          if(!x$na_allowed){
            cond<-any(is.na(time_start))
            if(cond){
              cond<-all(is.na(time_start))
              if(cond){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
              }else{
                rows<-which(is.na(time_start))
                for(rowid in rows){
                  errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
                }
              }
            }
          }
          valid_start<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_start)
          if(all(valid_start)){
            date_impossible<-as.Date(time_start)>Sys.Date()
            if(any(date_impossible)){
              rows<-which(date_impossible)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Date after today"))
              }
            }
          }else{
            rows<-which(!valid_start)
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Invalid date format"))
            }
          }
          
          data_names<-data_names[!data_names %in% usedName]
          
          #TIME END
          time_end<-date_cols$separated$time_end
          x<-rules[["time_end"]]
          allowed_names<-c(x$id,unlist(x$aliases))
          usedName<-allowed_names[allowed_names%in%data_names]
          ### NA VALUES PRESENCE
          if(!x$na_allowed){
            cond<-any(is.na(time_end))
            if(cond){
              cond<-all(is.na(time_end))
              if(cond){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
              }else{
                rows<-which(is.na(time_end))
                for(rowid in rows){
                  errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
                }
              }
            }
          }
          valid_end<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_end)
          if(all(valid_end)){
            date_impossible<-as.Date(time_end)>Sys.Date()
            if(any(date_impossible)){
              rows<-which(date_impossible)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Date after today"))
              }
            }
          }else{
            rows<-which(!valid_end)
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Invalid date format"))
            }
          }
          
          data_names<-data_names[!data_names %in% usedName]
          
          if(all(valid_start)&all(valid_end)){
            date_order<-as.Date(time_end)>=as.Date(time_start)
            if(!all(date_order)){
              rows<-which(!date_order)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column="-",category="Invalid date","Date end before date start"))
              }
            }
          }
          
        }
      }
    }
  } 
  
  #REPORTING ENTITY COLUMN
  INFO("Check reporting entity column")
  if(!is.null(reporting_entity_col)){
    
    print(reporting_entity_col)
    x<-reporting_entity_col
    
    ### COLUMNS PRESENCE
    allowed_names<-c(x$id,unlist(x$aliases))
    cond<-any(allowed_names%in%data_names)
    
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column=x$id,category="Missing mandatory column",message=sprintf("Column '%s' is missing",x$id)))
    }else{
      usedName<-allowed_names[allowed_names%in%data_names]
      checkedCol<-data[[usedName]]
      
      ### NA VALUES PRESENCE
      if(!x$na_allowed){
        cond<-any(is.na(checkedCol))
        if(cond){
          cond<-all(is.na(checkedCol))
          if(cond){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
          }else{
            rows<-which(is.na(checkedCol))
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
            }
          }
        }
      }
      
      ### VALUES VALIDITY
      #### VALUE IN ALLOWED VALUES
      INFO("Check reporting entity allowed values")
      print(x)
      if(!is.null(x$allowed_values)){
        ref<-unlist(x$allowed_values)
        cond<-any(!(unique(checkedCol[!is.na(checkedCol)])%in%ref))
        if(cond){
          errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row="-",column=usedName,category="Invalid reporting entity",message=sprintf("At least one reporting entity does not match the selected reporting entity ('%s') for the '%s' column.",x$allowed_values, usedName)))
        }
      }
    }
  }
  
  ### SUPPLEMENTARIES COLUMNS
  
  for(addcol in data_names){
    errors<-rbind(errors,data.frame(type="INFO",rule="I01",row="-",column=addcol,category="Column not used",message=sprintf("'%s' is not a mandatory column and will be skiped",addcol)))
  }
  
  ### VALIDITY RESULTS
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
  
  return(out)
  
}

#standardizeNames
standardizeNames<-function(file,format,rules){
  
  task<-jsonlite::read_json(rules)
  
  rules<-task$formats[[format]]$columns
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  rules_cols<-names(rules)
  data_names<-names(data)
  
  for (i in rules_cols){
    x<-rules[[i]]
    std_name<-x$id
    alt_names<-unlist(x$aliases)
    if(std_name%in%data_names){}else{
      if(any(alt_names%in%data_names)){
        usedName<-alt_names[alt_names%in%data_names]
        names(data)[names(data) == usedName] <- std_name
      }
    }
  }
  return(data)
}

#simplifiedToGeneric
simplifiedToGeneric<-function(file,type){
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  if(type=='catch'){
    newdata<-data%>%
      pivot_longer(c(catch_retained,catch_discarded,catch_nominal),names_to="measurement_type",values_to="measurement_value")%>%
      mutate(measurement_type=gsub("^.*?_","",measurement_type))%>%
      rename(measurement_unit=catch_unit)
    names(newdata)[names(newdata) == "catch_obs"] <- "measurement_obs"
  }
  
  if(type=='effort'){
    newdata<-data%>%
      mutate(measurement_type=NA)%>%
      mutate(measurement_value=effort)%>%
      rename(measurement_unit=effort_unit)
    names(newdata)[names(newdata) == "effort_obs"] <- "measurement_obs"
  }
  
  newdata<-newdata%>%
    mutate(measurement=type)%>%
    mutate(date=paste0(year,"-",period))%>%
    mutate(time_start=dateFormating(date,"start"),
           time_end=dateFormating(date,"end"),
           time=paste0(time_start,"/",time_end))
  
  newdata<-subset(newdata,select = names(newdata) %in% c("fishingfleet",
                                                         "flagstate",
                                                         "time",
                                                         "time_start",
                                                         "time_end", 
                                                         "geographic_identifier",
                                                         "geographic_coordinates",
                                                         "fleet_segment",
                                                         "gear_type",
                                                         "fishing_mode",
                                                         "species",
                                                         "school_type",
                                                         "measurement",
                                                         "measurement_type",
                                                         "measurement_value",
                                                         "measurement_unit",
                                                         "measurement_obs"))
  
  return(newdata)
}

#simplifiedToGeneric
simplifiedToGeneric<-function(file,type){
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  if(type=='catch'){
    newdata<-data%>%
      pivot_longer(c(catch_retained,catch_discarded,catch_nominal),names_to="measurement_type",values_to="measurement_value")%>%
      mutate(measurement_type=gsub("^.*?_","",measurement_type))%>%
      rename(measurement_unit=catch_unit)
    names(newdata)[names(newdata) == "catch_obs"] <- "measurement_obs"
  }
  
  if(type=='effort'){
    newdata<-data%>%
      mutate(measurement_type=NA)%>%
      mutate(measurement_value=effort)%>%
      rename(measurement_unit=effort_unit)
    names(newdata)[names(newdata) == "effort_obs"] <- "measurement_obs"
  }
  
  newdata<-newdata%>%
    mutate(measurement=type)%>%
    mutate(date=paste0(year,"-",period))%>%
    mutate(time_start=dateFormating(date,"start"),
           time_end=dateFormating(date,"end"),
           time=paste0(time_start,"/",time_end))
  
  newdata<-subset(newdata,select = names(newdata) %in% c("fishingfleet",
                                                         "flagstate",
                                                         "time",
                                                         "time_start",
                                                         "time_end", 
                                                         "geographic_identifier",
                                                         "geographic_coordinates",
                                                         "fleet_segment",
                                                         "gear_type",
                                                         "fishing_mode",
                                                         "species",
                                                         "school_type",
                                                         "measurement",
                                                         "measurement_type",
                                                         "measurement_value",
                                                         "measurement_unit",
                                                         "measurement_obs"))
  
  return(newdata)
}

#validateCallRules
validateCallRules <- function(file, rules){
  
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
  
  return(out)
  
}

#completeWithMissingEntities
completeWithMissingEntities<-function(config,data){
  reporting_entities<-config$dcf$reporting_entities$codelist_ref
  missing_entities<-subset(reporting_entities,!code%in%data$reporting_entity)$code
  if(length(missing_entities>0)){
    missing <- data.frame(
      id = rep("",length(missing_entities)),
      data_call_id = rep(unique(data$data_call_id),length(missing_entities)),
      data_call_folder = rep(unique(data$data_call_folder),length(missing_entities)),
      task_id = rep(unique(data$task_id),length(missing_entities)),
      reporting_entity = missing_entities,
      owner = rep("",length(missing_entities)),
      creationTime = rep(NA,length(missing_entities)),
      lastModifiedBy = rep("",length(missing_entities)),
      lastModificationTime = rep(NA,length(missing_entities)),
      status = rep("MISSING",length(missing_entities)),
      stringsAsFactors = FALSE)
    
    data<-rbind(data,missing)
  }
  return(data)
}

#getSubmissions
getSubmissions <- function(config, store, user_only = FALSE,data_calls_id = NULL,full_entities=FALSE){
  items <- store$listWSItems()
  workspace_filter <- paste0(config$dcf$workspace,"-")
  if(user_only) workspace_filter <- config$dcf$user_workspace
  user_folders <- items[sapply(items$title, startsWith, workspace_filter),]
  all_items <- do.call("rbind", lapply(1:nrow(user_folders), function(i){
    user_folder <- user_folders[i,]
    user_items <- store$listWSItemsByPath(folderPath = user_folder$path)
    user_submissions <- NULL
    if(length(user_items)>0){
      user_submissions <- do.call("rbind", lapply(1:nrow(user_items), function(j){
        user_item <- user_items[j,]
        
        data_call_folder <- basename(user_item$name)
        data_call_props <- unlist(strsplit(data_call_folder, "_for_"))
        reporting_entity <- data_call_props[2]
        data_call_props <- unlist(strsplit(data_call_props[1],"_"))
        data_call_id <- unlist(strsplit(data_call_props[1], "datacall-"))[2]
        task_id <- unlist(strsplit(data_call_props[2], "task-"))
        task_id <- paste0(task_id[2:length(task_id)], collapse = "task-")
        
        if(!is.null(data_calls_id)) if(!data_call_id%in%data_calls_id){
        user_submissions <- data.frame(
          id = character(0),
          data_call_id = character(0),
          data_call_folder = character(0),
          task_id = character(0),
          reporting_entity = character(0),
          owner = character(0),
          creationTime = character(0),
          lastModifiedBy = character(0),
          lastModificationTime = character(0),
          status = character(0),
          stringsAsFactors = FALSE
        )
        return(user_submissions)
        }
        
        #fetch metadata
        dcfile_item <- store$getWSItem(parentFolderID = user_item$id, itemPath = paste0(data_call_folder,".xml"))
        dcfile <- store$downloadItem(item = dcfile_item, wd = tempdir())
        dc_entry <- atom4R::readDCEntry(dcfile)
        status <- "SUBMITTED"
        if(!is.null(dc_entry$dateAccepted)){
          if(all(dc_entry$dateAccepted[[1]]$value != "NA")){
            status <- "ACCEPTED"
          }else{
            status <- "REJECTED"
          }
        }
        
        user_submission <- data.frame(
          id = user_item$id,
          data_call_id = data_call_id,
          data_call_folder = data_call_folder,
          task_id = task_id,
          reporting_entity = reporting_entity,
          owner = user_item$owner,
          creationTime = as.POSIXct(user_item$creationTime/1000, origin = "1970-01-01"),
          lastModifiedBy = user_item$lastModifiedBy,
          lastModificationTime = as.POSIXct(user_item$lastModificationTime/1000, origin = "1970-01-01"),
          status = status,
          stringsAsFactors = FALSE
        )
        return(user_submission)
      }))
    }else{
      user_submissions <- data.frame(
        id = character(0),
        data_call_id = character(0),
        data_call_folder = character(0),
        task_id = character(0),
        reporting_entity = character(0),
        owner = character(0),
        creationTime = character(0),
        lastModifiedBy = character(0),
        lastModificationTime = character(0),
        status = character(0),
        stringsAsFactors = FALSE
      )
    }
    return(user_submissions)
  }))
  
  if(full_entities){
    all_items<-completeWithMissingEntities(config,all_items)
  }
  return(all_items)
}

#acceptSubmission
acceptSubmission <- function(config, store, data_call_folder, data_submission_id){
  dcfile_item <- store$getWSItem(parentFolderID = data_submission_id, itemPath = paste0(data_call_folder,".xml"))
  dcfile <- store$downloadItem(item = dcfile_item, wd = tempdir())
  dc_entry <- atom4R::readDCEntry(dcfile)
  dc_entry$dateAccepted <- list()
  dc_entry$addDCDateAccepted(Sys.time())
  dc_entry$save(file = dcfile)
  store$uploadFile(folderID = data_submission_id, file = dcfile)
}

#rejectSubmission
rejectSubmission <- function(config, store, data_call_folder, data_submission_id){
  dcfile_item <- store$getWSItem(parentFolderID = data_submission_id, itemPath = paste0(data_call_folder,".xml"))
  dcfile <- store$downloadItem(item = dcfile_item, wd = tempdir())
  dc_entry <- atom4R::readDCEntry(dcfile)
  dc_entry$dateAccepted <- list()
  dc_entry$addDCDateAccepted(NA)
  dc_entry$save(file = dcfile)
  store$uploadFile(folderID = data_submission_id, file = dcfile)
}

#listItemsSubmission
listItemsSubmission <- function(store, submission_id){
  store$listWSItems(parentFolderID = submission_id, showHidden = FALSE)
}

#copyItemsSubmission
copyItemsSubmission <- function(store, data_submission_id, wd=tempdir()){
  items<-store$listWSItems(parentFolderID = data_submission_id, showHidden = FALSE)
  if(nrow(items)>0){
    items_info <- do.call("rbind", lapply(1:nrow(items), function(i){
      item <- items[i,]
      dcfile_item <- store$getWSItem(parentFolderID = data_submission_id, itemPath = item$name)
      dcfile <- store$downloadItem(item = dcfile_item, wd = wd)
      
      item_info <- data.frame(
        id = item$id,
        name = item$name,
        path = dcfile,
        stringsAsFactors = FALSE
      )
      return(item_info)
    }))
  }else{
    item_info <- data.frame(
      id = character(0),
      name = character(0),
      path = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(items_info)
}

#createDataCall
sendReminder <- function(pool,data_call_id,reporting_entity=NULL,role=NULL,config, profile){
  
  #check existing entities
  
  #recipients<-getDBUsersWithRole(pool,profile,role,reporting_entity)
  recipients<-getDBUsers(pool,profile,reporting_entity)
  print(recipients)
  #get datacall info
  data_call <- getDataCalls(pool, id_data_call = data_call_id)
  
  if(nrow(recipients)==0){
    sended <- FALSE
    attr(sended, "error") <- sprintf("There is currently no person assign for reporting entity '%s'", reporting_entity)
    return(sended)
  }

      INFO("Sending reminder notification to DB users")
      for(i in 1:nrow(recipients)){
        recipient <- recipients[i,]
        INFO("Sending data call notification to '%s'", recipient$username)
        sendMessage(
          subject = sprintf("[%s] Kindly reminder for Data call open for %s task ID '%s'", config$dcf$name, config$dcf$context, data_call$tasks),
          body = sprintf(
            "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            We haven't yet receipt your data for task ID '%s'.
            
            You are kindly invited to validate and submit your data before %s.
            
            Best regards,
            The %s
                         
            ",
            recipient$fullname, 
            config$dcf$name, config$dcf$context, config$dcf$roles$submitter, data_call$tasks,
            as(data_call$date_end,"character"),
            config$dcf$roles$manager
          ),
          recipients = as.list(recipient$username),
          profile = profile
        )
      }
      
      sended <- TRUE
      
  return(sended)
}
