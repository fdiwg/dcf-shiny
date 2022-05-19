#getDBUsers
getDBUsers <- function(pool){
  DBI::dbReadTable(pool, "dcf_users")
}