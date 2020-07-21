#' Main file for GloWPa model

# LIBRARIES
library(raster)
library(logger)

# SOURCE FILES
source("./Model scripts/population.R")
source("./Model scripts/wwtp.R")
source("./Model scripts/logger.R")
source("./Model scripts/emissions.R")
source("./Model scripts/plotter.R")
source("./Model scripts/geo.R")
source("./Model scripts/gadm.R")

#' SCENARIO 
#' run:                 identifier of scenario run
#' loadings_module:     1= human_emissions, 2= pathogenflows
#' pathogen:            name of pathogen. must be one of pathogen_inputs
#' model_input:         directory of model input. default relative path to model_input
#' model_output:        directory of model output. default relative path to model_output
#' isoraster_filename:  name of the isoraster
#' wwtp_available:      1=no treatment, 2= treatment, no location, 3= treatment, location of wwtp known
#' WWTPData_filename:   filename of wwtp input data in case wwtp_available = 3
#' 
SCENARIO <<- data.frame(
  run = 1,
  loadings_module=1,
  pathogen ="cryptosporidium",
  model_input ="./Model input", 
  model_output = "./Model output",
  isoraster_filename="isoraster.tiff",
  population_urban_filename = "popurban.tif",
  population_rural_filename = "poprural.tif",
  wwtp_available = 1,
  WWTPData_filename="",
  hydrology_available = FALSE,
  resolution = 0.008333,stringsAsFactors = F
  )

STATE <<- "not started"
ENV <<- list(
  csv_sep = ","
)
source("./local_env.R")
glowpa.run <- function(scenario,human_data,isoraster,popurban,poprural,wwtp_input=NULL){
  # merge defaults for scenario with scenario
  result = withCallingHandlers({
    SCENARIO <<- glowpa.get.params(scenario)
    log_file <- file.path(SCENARIO$model_output,sprintf("logs/log_run%s_pid%s.txt",SCENARIO$run, Sys.getpid()))
    logger.init(log_file)
    log_info('Start scenario run with id {SCENARIO$run}')
    params_table <- ""
    for(par in colnames(SCENARIO)){
      par_value <- SCENARIO[[par]]
      entry <- sprintf("%s= %s\n",par,par_value)
      params_table <- paste(params_table,entry)
    }
    log_info('Model started with following params:\n{params_table}')
    
    STATE <<- "running"
    ISORASTER <<- isoraster
    HUMAN_DATA <<- human_data
    OUTPUT <<- list(emissions=NULL,grid=NULL,files=list(pathogen_water_grid=NULL))
    dir.create(SCENARIO$model_output,recursive = T,showWarnings = F)
    # AREA_EXTENT <<- extent(ISORASTER)
    pathogen_inputs <- read.csv(file.path(SCENARIO$model_input,"pathogen_inputs.csv"), stringsAsFactors = FALSE)
    # search pathogen information
    pathogen_row <- which(pathogen_inputs$name==SCENARIO$pathogen)
    if(length(pathogen_row)<1){
      log_error("No pathogen information found for pathogen {SCENARIO$pathogen}")
      stop()
    }
    pathogen <- pathogen_inputs[pathogen_row,]
    populations <- population.preprocess(popurban,poprural)
    if(SCENARIO$loadings_module==1){
      log_error("Human emissions module not yet implemented in this version.")
      stop()
    }
    else if(SCENARIO$loadings_module==2){
      source("./Model scripts/pathogenflows.R")
      OUTPUT$emissions <<- pathogenflow.run(pathogen)
    }
    
    #calculate emissions pp
    OUTPUT$emissions <<- emissions.calc.avg.pp(OUTPUT$emissions)
    # replace na values with 0
    # TODO: check if this is needed in all cases
    OUTPUT$emissions <<- emissions.na.replace(OUTPUT$emissions)
    # apply wwtp
    wwtp_output <- wwtp.run(OUTPUT$emissions, pathogen, populations$urban, populations$rural, wwtp_input)
    OUTPUT$emissions <<- wwtp_output$emissions
    OUTPUT$grid <<- wwtp_output$grid
    if(SCENARIO$hydrology_available){
      stop("ERROR: Hydrology not implemented in this version")
    }
    else if(SCENARIO$loadings_module==1){
      # TODO: think about moving this to human_emissions.R specific code.
      OUTPUT$grid$pathogen_land <<-0.025*OUTPUT$grid$pathogen_land
      OUTPUT$grid$pathogen_water <<- OUTPUT$grid$pathogen_water + OUTPUT$grid$pathogen_land
      OUTPUT$emissions$pathogen_urb_dif <<-0.025*OUTPUT$emissions$pathogen_urb_dif
      OUTPUT$emissions$pathogen_rur_dif <<-0.025*OUTPUT$emissions$pathogen_rur_dif
      OUTPUT$emissions$pathogen_urb_onsite_land <<-0.025*OUTPUT$emissions$pathogen_urb_onsite_land
      OUTPUT$emissions$pathogen_rur_onsite_land <<-0.025*OUTPUT$emissions$pathogen_rur_onsite_land
      # save in asci grid format
      writeRaster(OUTPUT$grid$pathogen_water,file.path(SCENARIO$model_output,sprintf("humanemissions_%s%s",pathogen$name,SCENARIO$run)), format="ascii", overwrite=TRUE)
      
      library(tidyverse)
      library(dplyr)
      # save emissions to csv
      selection <- emissions %>% select(1:16)
      write.csv(selection, file=file.path(SCENARIO$model_output,sprintf("HumanEmissionsCalculated_%s%s.csv",pathogen$name,SCENARIO$run)))
      detach("package:tidyverse", unload=TRUE)
      detach("package:dplyr", unload=TRUE)
    }
    else if(SCENARIO$loadings_module==2){
      totals <- pathogenflow.calc.totals(OUTPUT$emissions)
      OUTPUT$emissions <<- totals
    }
    # save geotiff
    OUTPUT$grid$pathogen_water[OUTPUT$grid$pathogen_water==0]<-NA
    out_file <- file.path(SCENARIO$model_output,sprintf("humanemissions_%s%s.tif",pathogen$name,SCENARIO$run))
    log_info("Writing raster output")
    writeRaster(OUTPUT$grid$pathogen_water,out_file,format="GTiff",overwrite=TRUE)
    OUTPUT$files$pathogen_water_grid = out_file 
    
    STATE <<- "finished"
    log_info("finished scenario run with id {SCENARIO$run}")
    return(OUTPUT)
  }, warning = function(warning_condition) {
    log_warn("{warning_condition$message}")
  }, error = function(error_condition) {
   log_error("{error_condition}")
  })
  
}


glowpa.get.params <- function(scenario){
  # merge defaults in scenario
  for(col in colnames(SCENARIO)){
    if(!col %in% colnames(scenario)){
      scenario[[col]] <- SCENARIO[[col]]
    }
  }
  return(scenario)
}

