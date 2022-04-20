#getTasks
getTasks <- function(config){
  tasks <- config$dcf$tasks
  task_list <- names(tasks)
  task_list<- setNames(task_list, unlist(lapply(tasks, function(x){x$name})))
  return(task_list)
}