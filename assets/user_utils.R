#getDBUserReportingEntities
getDBUserReportingEntities <- function(pool, profile){
  conn <- pool::poolCheckout(pool)
  user_sql <- sprintf("SELECT * FROM dcf_users WHERE username = '%s'", profile$preferred_username)
  out_sql <- try(DBI::dbGetQuery(conn, user_sql))
  return(unlist(strsplit(out_sql$reporting_entities,",")))
}

#getDBUsers
getDBUsers <- function(pool){
  DBI::dbReadTable(pool, "dcf_users")
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