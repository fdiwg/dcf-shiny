#logger
logger <- function(type, txt, ...){
  log_txt <- sprintf(txt, ...)
  cat(sprintf("[dcf-shiny][%s] %s \n", type, log_txt), file = stderr())
}
INFO <- function(txt, ...){logger("INFO", txt, ...)}
WARN <- function(txt, ...){logger("WARN", txt, ...)}
ERROR <- function(txt, ...){logger("ERROR", txt, ...)}
DEBUG <- function(txt, ...){if(CONFIG$debug) logger("DEBUG", "\u203C", txt, ...)}

DEBUG_MODULE_PROCESSING_TIME <- function(module, start, end){
  module_time = end - start
  DEBUG("\u23F3 %s module loaded in %s %s", module, as(module_time, "numeric"), attr(module_time, "units"))
}