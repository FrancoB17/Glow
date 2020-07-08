#' Main file for GloWPa model

# LIBRARIES

library(raster)

# SOURCE FILES
source("./Model scripts/population.R")
source("./Model_scripts/wwtp.R")

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
  model_output = "./Model_output",
  isoraster_filename="isoraster.tiff",
  population_urban_filename = "popurban.tif",
  population_rural_filename = "poprural.tif",
  wwtp_available = 1,
  WWTPData_filename="",
  hydrology_available = FALSE
  )

STATE <<- "not started"

glowpa.run <- function(config,human_data){
  # TODO: merge config with global scenario options
  STATE <<- "running"
  ISORASTER <<- raster(file.path(SCENARIO$model_input,SCENARIO$isoraster_filename))
  HUMAN_DATA <<- human_data
  OUTPUT <<- list(emissions=NULL,grids=NULL,files=list())
  # AREA_EXTENT <<- extent(ISORASTER)
  pathogen_inputs <- read.csv(file="./Model input/pathogen_inputs.csv", stringsAsFactors = FALSE)
  # search pathogen information
  pathogen_row <- which(pathogen_inputs$name==scenario$pathogen)
  if(length(pathogen_row)<1){
    stop("Error: pathogen misspelled or not in list")
  }
  pathogen <- pathogen_inputs[pathogen_row,]
  populations <- population.preprocess()
  if(SCENARIO$loadings_module==1){
    stop("ERROR: Human emissions module not yet implemented in this version.")
  }
  else if(SCENARIO$loadings_module==2){
    source("./Model scripts/pathogenflows.R")
    OUTPUT$emissions <<- pathogenflows.run(pathogen)
  }
  
  wwtp_output <- wttp.run(OUTPUT$emissions, pathogen, populations$urban, populations$rural)
  OUTPUT$emissions <<- wttp_output$emissions
  OUTPUT$grid <<- wttp_output$grid
  if(SCENARIO$hydrology_available){
    stop("ERROR: Hydrology not implemented in this version")
  }
  else if(SCENARIO$loadings_module==1){
    OUTPUT$grid$pathogen_land_grid <<-0.025*OUTPUT$grid$pathogen_land_grid
    OUTPUT$grid$pathogen_water_grid <<- OUTPUT$grid$pathogen_water_grid + OUTPUT$grid$pathogen_land_grid
    OUTPUT$emissions$pathogen_urb_dif <<-0.025*OUTPUT$emissions$pathogen_urb_dif
    OUTPUT$emissions$pathogen_rur_dif <<-0.025*OUTPUT$emissions$pathogen_rur_dif
    OUTPUT$emissions$pathogen_urb_onsite_land <<-0.025*OUTPUT$emissions$pathogen_urb_onsite_land
    OUTPUT$emissions$pathogen_rur_onsite_land <<-0.025*OUTPUT$emissions$pathogen_rur_onsite_land
    # save in asci grid format
    writeRaster(OUTPUT$grid$pathogen_water_grid,file.path(SCENARIO$model_output,sprintf("humanemissions_%s%s",pathogen$name,SCENARIO$run)), format="ascii", overwrite=TRUE)
  }
  else if(SCENARIO$loadings_module==2){
    # TODO:
  }
  # save geotiff
  OUTPUT$grid$pathogen_water_grid[OUTPUT$grid$pathogen_water_grid==0]<-NA
  out_file <- file.path(SCENARIO$model_output,sprintf("humanemissions_%s%s",pathogen$name,SCENARIO$run))
  writeRaster(OUTPUT$grid$pathogen_water_grid,out_file,format="GTiff",overwrite=TRUE)
  OUTPUT$files$pathogen_water_grid <<- out_file 
  
  library(tidyverse)
  library(dplyr)
  # TODO: I think we need other routine here.
  # TODO: select data for output
  selection <- point %>% select(1:16)
  write.csv(selection, file=file.path(SCENARIO$model_output,sprintf("HumanEmissionsCalculated_%s%s.csv",pathogen$name,SCENARIO$run)))
  detach("package:tidyverse", unload=TRUE)
  detach("package:dplyr", unload=TRUE)
  return(OUTPUT)
}




