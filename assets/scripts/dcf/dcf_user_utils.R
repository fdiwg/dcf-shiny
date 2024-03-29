#getDBUserReportingEntities
getDBUserReportingEntities <- function(pool, profile){
  conn <- pool::poolCheckout(pool)
  user_sql <- sprintf("SELECT * FROM dcf_users WHERE username = '%s'", profile$preferred_username)
  out_sql <- try(DBI::dbGetQuery(conn, user_sql))
  out_entities <- unlist(strsplit(out_sql$reporting_entities,","))
  if(length(out_entities)==0) out_entities <- ""
  return(out_entities)
}

#getDBUserRoles
getDBUserRoles <- function(pool, profile){
  conn <- pool::poolCheckout(pool)
  user_sql <- sprintf("SELECT * FROM dcf_users WHERE username = '%s'", profile$preferred_username)
  out_sql <- try(DBI::dbGetQuery(conn, user_sql))
  out_roles <- unlist(strsplit(out_sql$roles,","))
  if(length(out_roles)==0) out_roles <- ""
  return(out_roles)
}

#fetchProfileRoles
fetchProfileRoles <- function(pool, profile){
  dbuser <- getDBUsers(pool, usernames = profile$preferred_username)
  if(nrow(dbuser)>0){
    profile$shiny_app_roles <- unlist(strsplit(dbuser$roles,","))
  }
  return(profile)
}

#getDBUsers
getDBUsers <- function(pool, profile = NULL, roles = NULL, reporting_entities = NULL,usernames = NULL){
  dcf_users <- tibble::as.tibble(DBI::dbReadTable(pool, "dcf_users"))
  #filters
  if(!is.null(roles)){
    pattern<-paste0(roles,collapse = "|")
    dcf_users<-dcf_users[grepl(pattern,dcf_users$roles),]
  }
  if(!is.null(reporting_entities)){
    pattern<-paste0(reporting_entities,collapse = "|")
    dcf_users<-dcf_users[grepl(pattern,dcf_users$reporting_entities),]
  }
  if(!is.null(usernames)){
    dcf_users<-dcf_users[dcf_users$username %in% usernames,]
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

#getUsers
getUsers <- function(pool,profile){
  out_users <- data.frame(
    username = character(0),
    fullname = character(0),
    db = character(0),
    id_user = character(0),
    stringsAsFactors = FALSE
  )
  out_vre <- httr::GET("https://api.d4science.org/rest/2/users/get-all-fullnames-and-usernames",
                       httr::add_headers("Authorization" = paste("Bearer", profile$access$access_token)))
  if(httr::status_code(out_vre)==200){
    out_c <- content(out_vre)$result
    out_users <- data.frame(
      username = names(out_c),
      fullname = as.character(out_c),
      db = rep(FALSE,length(out_c)),
      id_user = rep("",length(out_c)),
      stringsAsFactors = FALSE
    ) 
    db_users <- getDBUsers(pool)
    if(!is.null(db_users)) if(nrow(db_users)>0){
      out_users$db <- sapply(out_users$username, function(x){x %in% db_users$username})
      out_users$id_user <- sapply(out_users$username, function(x){if(x %in% db_users$username){db_users[db_users$username == x, "id_user"]}else{""}})
      out_users$roles <- sapply(out_users$username, function(x){if(x %in% db_users$username){db_users[db_users$username == x, "roles"]}else{""}})
      out_users$reporting_entities <- sapply(out_users$username, function(x){if(x %in% db_users$username){db_users[db_users$username == x, "reporting_entities"]}else{""}})
    }
  }
  return(out_users)
}

#createDBUser
createDBUser <- function(pool, username, fullname, roles = NULL, reporting_entities = NULL, profile){
  if(is.null(roles)) roles = ""
  if(is.null(reporting_entities)) reporting_entities = ""
  conn <- pool::poolCheckout(pool)
  idx <- max(getDBUsers(pool)$id_user)+1
  creation_date <- Sys.time()
  attr(creation_date, "tzone") <- "UTC"
  #db management
  insert_sql <- sprintf(
    "INSERT INTO dcf_users(id_user, username, fullname, roles, reporting_entities, creator_id, creation_date) 
           VALUES (%s, '%s', '%s', '%s', '%s', '%s', '%s');", 
    idx, username, gsub("'","''", fullname), paste0(roles,collapse=","), paste0(reporting_entities,collapse=","), profile$preferred_username, as(creation_date, "character")
  )
  print(insert_sql)
  out_sql <- try(DBI::dbSendQuery(conn, insert_sql))
  created <- !is(out_sql, "try-error")
  if(!created){
    attr(created, "error") <- as(out_sql, "character")
  }
  return(created)
}

#updateDBuser
updateDBUser <- function(pool, id_user, roles = NULL, reporting_entities = NULL, profile){
  if(is.null(roles)) roles = ""
  if(is.null(reporting_entities)) reporting_entities = ""
  conn <- pool::poolCheckout(pool)
  update_date <- Sys.time()
  attr(update_date, "tzone") <- "UTC"
  #db management
  update_sql <- sprintf("UPDATE dcf_users 
                               SET roles = '%s', reporting_entities = '%s', updater_id = '%s', update_date = '%s' 
                               WHERE id_user = %s",
                        paste0(roles, collapse=","), paste0(reporting_entities, collapse=","), profile$preferred_username, as(update_date, "character"), 
                        id_user)
  out_sql <- try(DBI::dbSendQuery(conn, update_sql))
  updated <- !is(out_sql, "try-error")
  if(!updated){
    attr(updated, "error") <- as(out_sql, "character")
  }
  return(updated)
}