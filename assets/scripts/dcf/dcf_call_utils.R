

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
  idx <- 0
  data_calls <- getDataCalls(pool)
  if(nrow(data_calls)>0) idx <- max(data_calls$id_data_call)+1
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
                        as(start,"character"), ifelse((prev_call$status=="OPENED"&status=="CLOSED"),as(Sys.Date()-1,"character"),as(end,"character")), status, profile$preferred_username, 
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
