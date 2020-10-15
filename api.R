#' This file can be used to run gloWPa using REST api calls. Start api by api <- plumber::plumb(api.R) and then api$run()
library(plumber)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
source("./Model scripts/readers.R")
source("./Model scripts/GloWPa.R")

#' @post /scenario
function(human_data,isoraster,popurban,poprural,wwtp,level,wkt_extent,pathogen_type,wwtp_available){
  browser()
  # wkt_extent <-  "POLYGON ((29.5715 -1.48214, 35.00027 -1.48214, 35.00027 4.234466, 29.5715 4.234466, 29.5715 -1.48214))"
  # TODO: isoraster, population grids will be stored on server???
  if(missing(human_data) || missing(isoraster) || missing(popurban) || missing(poprural) ){
    stop("Error: missing arguments")
  }
  # NOTE: somehow, missing and is.null cannot be combined
  if(is.null(human_data) || is.null(isoraster) || is.null(popurban) || is.null(poprural)){
    stop("Error: missing arguments")
  }
  human_data <- read.csv(text = human_data)
  cl <- makeCluster(detectCores())
  clusterExport(cl,varlist = c('readers.read.raster','raster','log_info','tic','toc','ENV'))
  grids <- parLapply(cl,list(iso=isoraster,urban=popurban,rural=poprural),fun = function(x){
    readers.read.raster(x)
  }) 
  stopCluster(cl)
  isoraster_grid <- grids$iso
  poprural_grid <- grids$rural
  popurban_grid <- grids$urban
  #isoraster_grid <- readers.read.raster(isoraster)
  if(missing(level)){
    gadm_level <- 0
  }
  else if(is.null(level)){
    gadm_level <- 0
  }
  else{
    gadm_level <- as.integer(level) 
  }
  # implementation for new setup
  # in this case gadm data will be read from the geopackage. 
  borders <- geo.get.boundaries(level=gadm_level,gpkg_path=ENV$gadm_file,wkt_filter=wkt_extent)
  
  # crop isoraster
  isoraster_grid <- crop(isoraster_grid,borders)

  wwtp_input <- NULL
  if(!missing(wwtp)){
    if(!is.null(wwtp)){
      wwtp_input <- readers.read.wwtp(wwtp)
    }
  }

  if(missing(pathogen_type)){
      pathogen_type <- "Virus"
  }
  else if(is.null(pathogen_type)){
    pathogen_type <- "Virus"
  }
  
  if(missing(wwtp_available)){
    wwtp_available <- 2
  }
  else if(is.null(wwtp_available)){
    wwtp_available <- 2
  }
  # create directory for output
  model_ouput <- file.path("./Model output/",Sys.getpid())
  dir.create(model_ouput,recursive = T,showWarnings = F)
  # setup scenario options for mapping tool

  # assume xres and yres are identical
  resolution <- raster::xres(isoraster_grid)
  
  scenario <- data.frame(
    pathogen_type = pathogen_type,
    pathogen_name = "",
    use_pathogen_file = TRUE,
    model_output = model_ouput, 
    resolution=resolution, 
    loadings_module=2,
    wwtp_available=wwtp_available,
    run=1,
    gadm_level = gadm_level,
    save_emissions=F,stringsAsFactors = F)
  glowpa_output <- glowpa.run(scenario[1,],human_data,isoraster_grid,popurban_grid,poprural_grid,wwtp_input)
  # overwrite raster with log10 values
  if(level != 4){
    glowpa_output$grid$pathogen_water <- log10(glowpa_output$grid$pathogen_water)
    writeRaster(glowpa_output$grid$pathogen_water,filename = glowpa_output$files$pathogen_water_grid, overwrite=T)
  } else {
    glowpa_output$emissions<-log10(glowpa_output$emissions)
  }
  brks<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,Inf)
  cols <- plotter.get.colors.khroma("discrete rainbow",18)
  boundaries_plot <- borders
  if(level==0){
    boundaries_plot <- rnaturalearth::ne_countries(scale=50,returnclass = "sp")
  }
  plot_extent <- as_Spatial(st_as_sfc(wkt_extent)) 
  if(level != 4){
    plot_path <- plotter.plot.map(glowpa_output$grid$pathogen_water,col=cols,breaks=brks,boundaries=boundaries_plot, extent=plot_extent)
    response <- list(
      grid=list(
        file=glowpa_output$files$pathogen_water_grid,
        min=minValue(glowpa_output$grid$pathogen_water), 
        max=maxValue(glowpa_output$grid$pathogen_water)),
      figs = c(plot_path), 
      emissions=glowpa_output$emissions)
  }else{
    plot_path <- plotter.plot.map.level4(glowpa_output$emissions,col=cols,breaks=brks,boundaries=boundaries_plot, extent=plot_extent)
    response <- list(
      figs = c(plot_path), 
      emissions=glowpa_output$emissions)
  }
  
  return(jsonlite::toJSON(response))
}

#' @get /state
function(){
  STATE
}

