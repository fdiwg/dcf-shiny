#getTasks
getTasks <- function(config){
  tasks <- config$dcf$tasks
  task_list <- names(tasks)
  task_list<- setNames(task_list, unlist(lapply(tasks, function(x){x$name})))
  return(task_list)
}

#eval_variable_expression
eval_variable_expression <- function(str){
  eval_str <- eval(parse(text = whisker::whisker.render(str, as.list(Sys.getenv()))))
  eval_str <- eval(parse(text = eval_str))
  return(eval_str)
}
#data_time
data_time <- function(start_year, end_year){
  sprintf("%s-01-01/%s-12-31", start_year, end_year)
}
#data_time_start
data_time_start <- function(year){
  sprintf("%s-01-01", year)
}
#data_time_end
data_time_end <- function(year){
  sprintf("%s-12-31", year)
}
#profile_property
profile_property <- function(property){
  PROFILE[[property]]
}
#profile_flagstate
profile_flagstate <- function(){
  profile_property("flagstate")
}
#profile_organization
profile_organization <- function(){
  profile_property("organization")
}
#testing
if(FALSE){
  #see example configs/dev/config.yml
  
  #to get from data call creation date
  #for better formalism within config we use environment variable instead of globally defined R object
  #the formalism is done with 'Mustache' templating technology with double braces
  Sys.setenv(DATA_CALL_YEAR = as.integer(format(Sys.Date(), "%Y"))) 
  #time
  eval_variable_expression("data_time({{DATA_CALL_YEAR}}-1,{{DATA_CALL_YEAR}}-1)")
  eval_variable_expression("1950,{{DATA_CALL_YEAR}}-2)")
  #time_start
  eval_variable_expression("data_time_start({{DATA_CALL_YEAR}}-1)")
  #time_end
  eval_variable_expression("data_time_end({{DATA_CALL_YEAR}}-1)")
  #flagstate
  eval_variable_expression("profile_flagstate()")
}
#