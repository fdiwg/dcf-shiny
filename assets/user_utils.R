#getDBUserReportingEntities
getDBUserReportingEntities <- function(pool, profile){
  conn <- pool::poolCheckout(pool)
  user_sql <- sprintf("SELECT * FROM dcf_users WHERE username = '%s'", profile$preferred_username)
  out_sql <- try(DBI::dbGetQuery(conn, user_sql))
  out_entities <- unlist(strsplit(out_sql$reporting_entities,","))
  if(length(out_entities)==0) out_entities <- ""
  return(out_entities)
}

#getDBUsers
getDBUsers <- function(pool, profile = NULL,reporting_entities=NULL,usernames=NULL){
  dcf_users <- tibble::as.tibble(DBI::dbReadTable(pool, "dcf_users"))
  if(nrow(dcf_users)>0) if(!is.null(profile)){
    user_roles <- getAppUserRoles(profile)
    dcf_users$roles <- sapply(1:nrow(dcf_users), function(i){
      roles <- list()
      dcf_user = dcf_users[i,]
      app_user <- user_roles[names(user_roles)==dcf_user$username]
      if(length(app_user)>0){
        app_user <- app_user[[1]]
        if(length(app_user$roles)>0){
          roles <- sapply(app_user$roles, function(x){x$name})
        }
      }
      return(roles)
    })
  }
  if(!is.null(reporting_entities)){
    pattern<-paste0(reporting_entities,collapse = "|")
    dcf_users<-dcf_users[grepl(pattern,dcf_users$reporting_entities),]
  }
  if(!is.null(usernames)){
    dcf_users<-dcf_users[dcf_users$username%in%usernames,]
  }
  return(dcf_users)
}

#getDBUsersWithRole
getDBUsersWithRole <- function(pool, profile, role,reporting_entities=NULL){
  users <- getDBUsers(pool, profile)
  if(nrow(users)>0){
    users <- users[sapply(users$roles, function(x){ role %in% x }),]
    if(!is.null(reporting_entities)){
      pattern<-paste0(reporting_entities,collapse = "|")
      users<-users[grepl(pattern,users$reporting_entities),]
    }
  }
  return(users)
}

#createDBUser
createDBUser <- function(pool, username, fullname, reporting_entities = NULL, profile){
  if(is.null(reporting_entities)) reporting_entities = ""
  conn <- pool::poolCheckout(pool)
  idx <- nrow(getDBUsers(pool))+1
  creation_date <- Sys.time()
  attr(creation_date, "tzone") <- "UTC"
  #db management
  insert_sql <- sprintf(
    "INSERT INTO dcf_users(id_user, username, fullname, reporting_entities, creator_id, creation_date) 
           VALUES (%s, '%s', '%s', '%s', '%s', '%s');", 
    idx, username, fullname, paste0(reporting_entities,collapse=","), profile$preferred_username, as(creation_date, "character")
  )
  out_sql <- try(DBI::dbSendQuery(conn, insert_sql))
  created <- !is(out_sql, "try-error")
  if(!created){
    attr(created, "error") <- as(out_sql, "character")
  }
  return(created)
}

#updateDBuser
updateDBUser <- function(pool, id_user, reporting_entities = NULL, profile){
  if(is.null(reporting_entities)) reporting_entities = ""
  conn <- pool::poolCheckout(pool)
  update_date <- Sys.time()
  attr(update_date, "tzone") <- "UTC"
  #db management
  update_sql <- sprintf("UPDATE dcf_users 
                               SET reporting_entities = '%s', updater_id = '%s', update_date = '%s' 
                               WHERE id_user = %s", 
                        paste0(reporting_entities, collapse=","), profile$preferred_username, 
                        as(update_date, "character"), id_user)
  out_sql <- try(DBI::dbSendQuery(conn, update_sql))
  updated <- !is(out_sql, "try-error")
  if(!updated){
    attr(updated, "error") <- as(out_sql, "character")
  }
  return(updated)
}