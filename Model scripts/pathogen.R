
pathogen.get <- function(){
  
  pathogen <- NULL
  pathogen_input_file <- file.path(SCENARIO$model_input,"pathogen_inputs.csv")
  if(!file.exists(pathogen_input_file)){
    pathogen_input_file <- "./Model input/pathogen_inputs.csv"
    log_warn("No pathogen_inputs.csv found in {SCENARIO$model_input}. Using {pathogen_input_file}")
  }
  if(!file.exists(pathogen_input_file) && SCENARIO$use_pathogen_file){
    log_error("Missing pathogen input file.")
    stop("Missing pathogen input file")
  }
  else if(SCENARIO$use_pathogen_file){
    pathogen_inputs <- read.csv(file.path(SCENARIO$model_input,"pathogen_inputs.csv"), stringsAsFactors = FALSE)
    if(SCENARIO$pathogen_name!=''){
      pathogen_row <- which(pathogen_inputs$name==SCENARIO$pathogen_name)
    }
    else{
      pathogen_row <- which(pathogen_inputs$pathogenType==SCENARIO$pathogen_type) 
    }
    if(length(pathogen_row)<1){
      log_error("No pathogen information found for pathogen {SCENARIO$pathogen_type} {SCENARIO$pathogen_name}")
      stop()
    }
    else if(length(pathogen_row)>1){
      log_warn("Multiple options found to select pathogen from pathogen inputs file.")
      pathogen_row <- pathogen_row[1]
    }
    pathogen <- pathogen_inputs[pathogen_row,]
  }
  # assume pathogen_type is given and HUMAN_DATA contains incidence, shreddingrate and shredding duration for pathogen types.
  else{
    # no other information is needed in this case
    pathogen <- data.frame(pathogenType=SCENARIO$pathogen_type, name="")
  }
  pathogen_params <- logger.get.table(pathogen)
  log_info("Model initialized with following pathogen params: \n{pathogen_params}")
  return(pathogen)
}

  