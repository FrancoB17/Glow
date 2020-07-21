#' This file can be used to run gloWPa using REST api calls. Start api by api <- plumber::plumb(api.R) and then api$run()
library(plumber)
library(jsonlite)
source("./Model scripts/readers.R")
source("./Model scripts/GloWPa.R")

#' @post /scenario
function(human_data,isoraster,popurban,poprural,wwtp,level){
  browser()
  # TODO: isoraster, population grids will be stored on server???
  if(missing(human_data) || missing(isoraster) || missing(popurban) || missing(poprural)){
    stop("Error: missing arguments")
  }
  human_data <- read.csv(text = human_data)
  isoraster_grid <- readers.read.raster(isoraster)
  if(missing(level)){
    gadm_level <- 0
  }
  else{
    gadm_level <- level
  }
 
  # implementation for new setup
  if('gid' %in% colnames(human_data)){
    # in this case gadm data will be read from the geopackage. 
    boundaries <- geo.get.boundaries(level=gadm_level,gadm_gids = human_data$gid,gpkg_path=ENV$gadm_file)
  }
  # implementation for pilot case uganda. TODO: remove this part when data has been updated with new columns
  else{
    # TODO: make this generic. We need the level to passed to the model from the api
    gadm_level <- 3
    boundaries <- geo.get.boundaries(level=gadm_level,country='UGA')
  }
  # crop isoraster
  isoraster_grid <- crop(isoraster_grid,boundaries)
  popurban_grid <- readers.read.raster(popurban)
  poprural_grid <- readers.read.raster(poprural)
  wwtp_input <- NULL
  if(!missing(wwtp)){
    wwtp_input <- readers.read.wwtp(wwtp)
  }
  # create directory for output
  model_ouput <- file.path("./Model output/",Sys.getpid())
  dir.create(model_ouput,recursive = T,showWarnings = F)
  # setup scenario options for mapping tool
  scenario <- data.frame(
    pathogen="cryptosporidium",
    model_output = model_ouput, 
    resolution=0.008333, 
    loadings_module=2,
    wwtp_available=3,
    run=1, stringsAsFactors = F)
  glowpa_output <- glowpa.run(scenario[1,],human_data,isoraster_grid,popurban_grid,poprural_grid,wwtp_input)
  # overwrite raster with log10 values
  glowpa_output$grid$pathogen_water <- log10(glowpa_output$grid$pathogen_water)
  writeRaster(glowpa_output$grid$pathogen_water,filename = glowpa_output$files$pathogen_water_grid, overwrite=T)
  brks<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,Inf)
  browser()
  cols <- plotter.get.colors.khroma("discrete rainbow",14)
  browser()
  plot_path <- plotter.plot.map(glowpa_output$grid$pathogen_water,col=cols,breaks=brks,boundaries=boundaries)
  response <- list(
    grid=list(
      file=glowpa_output$files$pathogen_water_grid,
      min=minValue(glowpa_output$grid$pathogen_water), 
      max=maxValue(glowpa_output$grid$pathogen_water)),
    figs = c(plot_path), 
    emissions=glowpa_output$emissions)
  return(jsonlite::toJSON(response))
}

#' @get /state
function(){
  STATE
}

