
logger.init <- function(log_file=NULL){
  if(!is.null(log_file)){
    if(!dir.exists(dirname(log_file))){
      dir.create(dirname(log_file),recursive = T)
    }
    file.create(log_file)
    log_appender(appender_file(log_file),index = 2)
  }
  log_appender(appender_console)
  log_format <- layout_glue_generator(format = '{level} {time} {namespace}/{fn}: {msg}')
  log_layout(log_format)
  log_threshold(INFO)
  session_info <- sessionInfo()
  log_info("R version: {session_info$R.version$version.string}")
  log_info("platform {session_info$R.version$platform}")
  for(package in session_info$otherPkgs){
    log_info("Loaded package {package$Package} {package$Version} ")
  }
}