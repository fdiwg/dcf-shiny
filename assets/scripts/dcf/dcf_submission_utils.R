#getSubmissions
getSubmissions <- function(config,pool,profile, store, user_only = FALSE,data_calls_id = NULL,full_entities=FALSE,status=NULL,reporting_entities=NULL){
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
          temporal_extent = character(0),
          submitter = character(0),
          creationTime = character(0),
          lastModifiedBy = character(0),
          lastModificationTime = character(0),
          status = character(0),
          stringsAsFactors = FALSE
        )
        return(user_submissions)
        }
        
        if(!is.null(reporting_entities)) if(!reporting_entity%in%reporting_entities){
          user_submissions <- data.frame(
            id = character(0),
            data_call_id = character(0),
            data_call_folder = character(0),
            task_id = character(0),
            reporting_entity = character(0),
            temporal_extent = character(0),
            submitter = character(0),
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
        lastModified <- as.POSIXct(dcfile_item$lastModificationTime/1000, origin = "1970-01-01")
        dcfile <- store$downloadItem(item = dcfile_item, wd = tempdir())
        dc_entry <- atom4R::readDCEntry(dcfile)
        submission_status <- "SUBMITTED"
        if(!is.null(dc_entry$dateAccepted)){
          if(is.list(dc_entry$dateAccepted)){
            if(all(dc_entry$dateAccepted[[1]]$value != "NA")){
              submission_status <- "ACCEPTED"
            }else{
              submission_status <- "REJECTED"
            }
          }else{
            #backward compatibility
            if(dc_entry$dateAccepted != "NA"){
              submission_status <- "ACCEPTED"
            }else{
              submission_status <- "REJECTED"
            }
          }
        }
        temporal_extent <- "-"
        if(!is.null(dc_entry$temporal)){
          temporal_extent <- dc_entry$temporal[[1]]$value
        }
        
        if(!is.null(status)) if(!submission_status%in%status){
          user_submissions <- data.frame(
            id = character(0),
            data_call_id = character(0),
            data_call_folder = character(0),
            task_id = character(0),
            reporting_entity = character(0),
            temporal_extent = character(0),
            submitter = character(0),
            creationTime = character(0),
            lastModifiedBy = character(0),
            lastModificationTime = character(0),
            status = character(0),
            stringsAsFactors = FALSE
          )
          return(user_submissions)
        }
        
        user_submission <- data.frame(
          id = user_item$id,
          data_call_id = data_call_id,
          data_call_folder = data_call_folder,
          task_id = task_id,
          reporting_entity = reporting_entity,
          temporal_extent = temporal_extent,
          submitter = user_item$owner,
          creationTime = as.POSIXct(user_item$creationTime/1000, origin = "1970-01-01"),
          lastModifiedBy = user_item$lastModifiedBy,
          lastModificationTime = lastModified,
          status = submission_status,
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
        temporal_extent = character(0),
        submitter = character(0),
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
    all_items<-completeWithMissingEntities(config,pool,profile,all_items,user_only)
  }
  return(all_items)
}

#acceptSubmission
acceptSubmission <- function(config,pool,profile, store, data_call_folder, data_submission_id,task,data_call_id,reporting_entity,usernames,comment=""){
  
  #unicity of accepted submission
  accepted_submission<-getSubmissions(config = config, pool = pool, profile = profile, store = store, user_only = FALSE,data_calls_id=data_call_id,status="ACCEPTED",reporting_entities = reporting_entity,full_entities=FALSE)
  
  if(nrow(accepted_submission)>0){
    accept <- FALSE
    attr(accept, "error") <- "A submission is already accepted for this reporting_entity, please reject this one first"
    return(accept)
  }
  
  dcfile_item <- store$getWSItem(parentFolderID = data_submission_id, itemPath = paste0(data_call_folder,".xml"))
  dcfile <- store$downloadItem(item = dcfile_item, wd = tempdir())
  dc_entry <- atom4R::readDCEntry(dcfile)
  dc_entry$dateAccepted <- list()
  dc_entry$addDCDateAccepted(Sys.time())
  dc_entry$save(file = dcfile)
  store$uploadFile(folderID = data_submission_id, file = dcfile)
  
  recipients<-getDBUsers(pool=pool,profile=profile,usernames=usernames)
  print(recipients)
  #accept Notification
  if(nrow(recipients)==0){
    sent <- FALSE
    return(sent)
  }
  
  INFO("Sending submission acceptation notification to DB users")
  sent <- all(do.call("c", lapply(1:nrow(recipients), function(i){
    recipient <- recipients[i,]
    INFO("Sending data call notification to '%s'", recipient$username)
    notif_sent <- sendMessage(
      subject = sprintf("[%s] Your submission for %s task ID '%s' has been accepted", config$dcf$name, config$dcf$context, task),
      body = sprintf(
        "Dear %s,
            
            Your data submission for task ID '%s' has been accepted by the %s with following comment :
            
            %s
            
            Best regards,
            The %s
                         
            ",
        recipient$fullname, 
        task,config$dcf$roles$manager,
        
        comment,
        
        config$dcf$roles$manager
      ),
      recipients = as.list(recipient$username),
      profile = profile
    )
    return(notif_sent)
  })))
  
  return(sent)
  
}

#rejectSubmission
rejectSubmission <- function(config,pool,profile, store, data_call_folder, data_submission_id,task,usernames,end,comment=""){
  dcfile_item <- store$getWSItem(parentFolderID = data_submission_id, itemPath = paste0(data_call_folder,".xml"))
  dcfile <- store$downloadItem(item = dcfile_item, wd = tempdir())
  dc_entry <- atom4R::readDCEntry(dcfile)
  dc_entry$dateAccepted <- list()
  dc_entry$addDCDateAccepted(NA)
  dc_entry$save(file = dcfile)
  store$uploadFile(folderID = data_submission_id, file = dcfile)
  
  recipients<-getDBUsers(pool=pool,profile=profile,usernames=usernames)
  #accept Notification
  if(nrow(recipients)==0){
    sent <- FALSE
    return(sent)
  }
  
  INFO("Sending submission reject notification to DB users")
  sent <- all(do.call("c", lapply(1:nrow(recipients), function(i){
    recipient <- recipients[i,]
    INFO("Sending data call notification to '%s'", recipient$username)
    notif_sent <- sendMessage(
      subject = sprintf("[%s] Your submission for %s task ID '%s' has been rejected", config$dcf$name, config$dcf$context, task),
      body = sprintf(
        "Dear %s,
            
            Your data submission for task ID '%s' has been rejected by the %s with following comment :
            
            %s
            
            You are kindly invited to provide modification according to %s comment and submit a new version before %s. 
            
            Best regards,
            The %s
                         
            ",
        recipient$fullname, 
        task,config$dcf$roles$manager,
        
        comment,
        
        config$dcf$roles$manager,end,
        
        config$dcf$roles$manager
      ),
      recipients = as.list(recipient$username),
      profile = profile
    )
    return(notif_sent)
  })))
  
  return(sent)
  
}

#deleteSubmission
deleteSubmission <- function(config,pool,profile, store, data_call_folder,data_call_id,task,reporting_entity){
  
  deleted <- store$deleteItem(itemPath = file.path(config$dcf$workspace, data_call_folder))
  
  recipients<- getDBUsersWithRole(pool = pool, profile = profile, role = config$dcf$roles$manager)
  #accept Notification
  if(nrow(recipients)==0){
    sent <- FALSE
    return(sent)
  }
  
  INFO("Sending submission delete notification to DB manager")
  sent <- all(do.call("c", lapply(1:nrow(recipients), function(i){
    recipient <- recipients[i,]
    INFO("Sending data call notification to '%s'", recipient$username)
    notif_sent <- sendMessage(
      subject = sprintf("[%s] New data submission for '%s' - task ID '%s' - reporting entity '%s' has been deleted",
                        config$dcf$name, config$dcf$context, task,reporting_entity),
      body = sprintf(
        "Dear %s,
            
			      You receive this notification because you are assigned as part of the %s (%s) as %s.
                                           
            The previous data submitted by %s (as %s) for data call '%s' - task '%s' - reporting entity '%s' ahas been deleted by this user.          
            
            Best regards,
            The system bot
                         
            ",
        recipient$fullname, 
        config$dcf$name, config$dcf$context, config$dcf$roles$manager, 
        profile$name, config$dcf$roles$submitter, data_call_id, task, reporting_entity),
    recipients = as.list(recipient$username),
    profile = profile
  )
  return(notif_sent)
  })))

return(sent)

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
        description = item$description,
        path = dcfile,
        stringsAsFactors = FALSE
      )
      return(item_info)
    }))
  }else{
    item_info <- data.frame(
      id = character(0),
      name = character(0),
      description = character(0),
      path = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(items_info)
}

#sendReminder
sendReminder <- function(pool,data_call_id,task=NULL,reporting_entity=NULL,date_end=NULL,role=NULL,config, profile){

  #check existing entities
  
  #recipients<-getDBUsersWithRole(pool,profile,role,reporting_entity)
  recipients<-getDBUsers(pool,profile,reporting_entity)
  print(recipients)
  #get datacall info
  if(is.null(task))task <- getDataCalls(pool, id_data_call = data_call_id)$task_id
  
  if(nrow(recipients)==0){
    sent <- FALSE
    message<-sprintf("There is currently no person assign for reporting entity '%s'", reporting_entity)
    attr(sent, "error") <- 
    INFO(message)
    return(sent)
  }

      INFO("Sending reminder notification to DB users")
      sent <- all(do.call("c", lapply(1:nrow(recipients), function(i){
        recipient <- recipients[i,]
        INFO("Sending data call notification to '%s'", recipient$username)
        notif_sent <- sendMessage(
          subject = sprintf("[%s] Kind reminder for Data call open for %s task ID '%s' - reporting_entity '%s'", config$dcf$name, config$dcf$context, task,reporting_entity),
          body = sprintf(
            "Dear %s,
            
            You receive this notification because you are assigned as part of the %s (%s) as %s.
            
            We didn't yet receive your data for task ID '%s' for reporting entity '%s'.
            
            You are kindly invited to validate and submit your data before %s.
            
            Best regards,
            The %s
                         
            ",
            recipient$fullname, 
            config$dcf$name, config$dcf$context, config$dcf$roles$submitter, task,reporting_entity,
            as(date_end,"character"),
            config$dcf$roles$manager
          ),
          recipients = as.list(recipient$username),
          profile = profile
        )
        return(notif_sent)
      })))
      
  return(sent)
}
