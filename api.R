#' This file can be used to run gloWPa using REST api calls. Start api by api <- plumber::plumb(api.R) and then api$run()
library(plumber)
library(jsonlite)
source("./Model scripts/readers.R")
source("./Model scripts/GloWPa.R")

#' @post /scenario
function(human_data,isoraster,popurban,poprural,wwtp){
  if(missing(human_data) || missing(isoraster) || missing(popurban) || missing(poprural) || missing(wwtp)){
    stop("Error: missing arguments")
  }
  human_data <- read.csv(text = human_data)
  isoraster_grid <- readers.read.raster(isoraster)
  popurban_grid <- readers.read.raster(popurban)
  poprural_grid <- readers.read.raster(poprural)
  wwtp_input <- NULL
  browser()
  if(!missing(wwtp)){
    wwtp_input <- readers.read.wwtp(wwtp)
  }
  
  # results <- glowpa.run(params,human_data,isoraster,popurban_grid,poprural_grid,wwtp_input)
  # # TODO: emissions to json
  # # TODO: geotiff location in response
  # list(ouput = "output here")
  model_ouput <- file.path("./Model output/",Sys.getpid())
  dir.create(model_ouput,recursive = T,showWarnings = F)
  scenario <- data.frame(pathogen="cryptosporidium",model_output = model_ouput, resolution=0.008333, loadings_module=2,wwtp_available=3,run=1)
  glowpa_output <- glowpa.run(scenario,human_data,isoraster_grid,popurban_grid,poprural_grid,wwtp_input)
  # overwrite raster with log10 values
  glowpa_output$grid$pathogen_water <- log10(glowpa_output$grid$pathogen_water)
  writeRaster(glowpa_output$grid$pathogen_water,filename = glowpa_output$files$pathogen_water_grid, overwrite=T)
  response <- list(grid=list(
    file=glowpa_output$files$pathogen_water_grid, 
    min=minValue(glowpa_output$grid$pathogen_water), 
    max=maxValue(glowpa_output$grid$pathogen_water)),
      emissions=glowpa_output$emissions)
  return(jsonlite::toJSON(response))
}

#' @get /state
function(){
  STATE
}

