#loadDBI
loadDBI <- function(config){
  #DB
  POOL <- try(pool::dbPool(
    drv = DBI::dbDriver(config$dbi$drv),
    dbname = config$dbi$dbname,
    host = config$dbi$host,
    port = config$dbi$port,
    user = config$dbi$user,
    password = config$dbi$password
  ))
  attr(POOL, "description") <- "Database"
  return(POOL)
}