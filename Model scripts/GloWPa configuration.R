# READ LIBS -------------------------------------------------------

library(sp)
library(rgeos)
library(maptools)
library(rworldmap)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rgdal)

# CONSTANTS -------------------------------------------------

# Is this used somewhere?
MONS <<- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")


# FUNCTIONS ---------------------------------------------------------------


model.start <- function(input,loadings_module,scenario_mode){
  # load functions from scripts

  source(file.path(working.path.script,"Human_emissions.R"))

  if(loadings_module==2){
    source(file.path(working.path.in,"Pathogenflows.R"))
  }
  if(scenario_mode){
    # run scenarios with original human emissions
    # run.scenarios will call run() for each scenario
    run.scenarios(input,loadings_module)
  }
  else{
    # online tool with pathogenflows module
    # TODO: @Nynke: How to handle isoraster in this case?
    isoraster = "?" # @Nynke where do the isorasters come from in case of the online tool?
    pathogen = "?" # @Nynke where is the pathogen information in case of the online tool?
    run(input,pathogen,isoraster,loadings_module)
  }
}


model.run <- function(input,pathogen,isoraster,loadings_module,run_id=NULL){
  # 1) calculate loadings
  if(loadings_module==1){
    loadings <- human.emissions.model.run(input,pathogen,isoraster)
  }
  else if(loadings_module==2){
    
    source(file.path(working.path.script,"Pathogenflows.R"))
    # online tool input != onsitedata. Extract urban/rural data in pathogenflows scripts
    loadings <- pathogenflow.model.run(input,pathogen)

    ## @Nynke: wwtp and rasterization also in human_emissions.R. It would be nice if we can use same functions for it, but the input format (column names) is different
    ## in mapping tool input and Human_Data.csv.  
    # 2) apply wwtp if available
    
    # 3) emissions -> raster
  }
  

  
  # 4) maps, figs
  
  # 5) hydrology
  
  # 6) concentrations
}


#' Run GloWPa scenarios
#' Run GlowPa from configuration.
#' @param config {data.frame} - Configuration of the simulation runs
#' @param iso_read_format {int} - 1 for polygons, 2 for raster
#'
#' @return
#' @export
#'
#' @examples
run.scenarios <- function(config, iso_read_format){
  nruns <-length(config$run)
  #read in the pathogen specific input data
  pathogen_inputs <- read.csv(file.path(working.path.in.pathogens,"pathogen_inputs.csv"),stringsAsFactors=FALSE)
  if(nruns>0){
    # LOOP RUNS
    for(i in 1:nruns){
      scenario <- config[i,]
      # POLYGONS TO RASTER
      if(iso_read_format==1 && which(names(scenario)=="first_time_area") == 0){
        stop("missing required property 'first_time_area' in scenario")
      }
      else if(iso_read_format==1 && scenario$first_time_area){
        # makeisogrids.R needs to be run only the first time when you have polygon data. Afterwards, the isoreadformat can be
        # set to 2, as the isogrids file has been produced already.
        # In case the shape file is not in a standard lat-lon projection (WGS84), they need to also be transformed
        # that does currently not happen in the makeisogrids program.
        source(file.path(working.path.script,"makeisogrids.R"))
        make.iso.grids(scenario$layer, scenario$resolution)
        # Raster has been made, switch to raster mode
        iso_read_format <- 2
      }
      # The isoraster needs to be prepared before running the model. This raster is a file that links 
      # an area on the map to the district number it is located in.
      isoraster<<-raster(paste0(working.path.in,scenario$isoraster_filename)) #now read the isogrids
      
      # check that the correct resolution isoraster data have been read in
      if((xres(isoraster))>1e-5 || (yres(isoraster))>1e-5){
        stop("ERROR: isoraster file has incorrect resolution!")
      }
      
      # livestock switch - for now switched off. Later on the model can be adapted to include also livestock
      livestock_switch <- scenario$livestock_switch
      
      #select the pathogen:
      pathogen_name <- scenario$pathogen #select pathogen. Options: rotavirus, cryptosporidium
      
      #later on we may want to set an option that several pathogens can be selected at the same time -> in that case the loop below should be run multiple times with different input variables (easiest). This won't be the quickest option. It would be much quicker to run them simultaneiously, as for example the distribution over grids, the hydrology etc will be the same.
      if(pathogen_name=="cryptosporidium" && !livestock_switch){
        warning(sprintf("Warning: the pathogen %s also has a livestock source. Please be aware the current model only simulates the human source",pathogen_name))
      }
      
      pathogen_row <- which(pathogen_inputs$name==pathogen_name)
      if(length(pathogen_row)<1){
        stop("Error: pathogen misspelled or not in list")
      }
      pathogen <- pathogen_inputs[pathogen_row,]
      # is local - area specific incidence data available?
      incidence_available <-scenario$incidence_available
      if(incidence_available){
        pathogen$incidence_highhdi_over5 <-1 #change number - in case you are in a lowHDI area, you can leave this
        pathogen$incidence_highhdi_under5 <-1 #change number - in case you are in a lowHDI area, you can leave this
        pathogen$incidence_lowhdi_over5 <-1 #change number - in case you are in a highHDI area, you can leave this
        pathogen$incidence_lowhdi_under5 <-1 #change number - in case you are in a highHDI area, you can leave this
        print("Constand incidence is used throughout the study area. Only in larger areas HDI is used to distinguish areas with high and low incidence")
      }
      else{
        print("Generic literature values for pathogen incidence will be used. The results are therefore average background emissions and concentrations.")
      }
      
      # scenarios will run with original human emissions module
      model.run(scenario,pathogen,isoraster,1,scenario$run)
      # Commented out hydrology and wwtp preperations, because it should be done outside scenario mode
      
      #is gridded hydrological data available? Required are discharge, runoff, water level or depth, flow direction, residence time of water in each grid, and possibly water temperature and shortwave radiation to be consistent
      # hydrology_available<<-scenario$hydrology_available
      # if(identical(hydrology_available,FALSE) & isTRUE(overall_inputs$interest_concentrations)){
      #   #run the Wflow model in python - don't know how yet
      #   print("WARNING: Only emissions can be estimated, no concentations yet - this will change once we have the Wflow model running")
      # }
      # print(pathogen$name)
      # if(hydrology_available){
      #   discharge_files <<- list.files(path=(file.path(working.path.in,"discharge")))
      #   discharge <<-raster(file.path(working.path.in,"discharge",discharge_files[1]))
      #   discharge <<-crop(discharge,extent(isoraster))
      #   isoraster_hydro <<- crop(isoraster,extent(discharge))
      #   res_multiplier<<-xres(discharge)/xres(isoraster)
      #   if(res_multiplier>1){
      #     isoraster_hydro<<-aggregate(isoraster_hydro,fact=res_multiplier,fun=modal,na.rm=TRUE)
      #   }else if (res_multiplier<1){
      #     print("ERROR: THE RESOLUTION OF THE HYDROLOGY DATA IS LOWER THAN THE SET RESOLUTION AND THE MODEL HAS NOT BEEN SET UP TO CORRECT FOR THIS!")
      #   }
      # }
      # 
      # #is location-specific WWTP data available?
      # wwtp_available <<- scenario$wwtp_available
      # if(wwtp_available==3){
      #   print("Emissions from the population connected to a sewer will be entered in a single grid (also the sewage not treated). Leaky pipes are therefore missed. This option is only realistic when for the full area covered has data on WWTP location")
      # }else if(wwtp_available==2){
      #   print("The emissions from sewers will be spread over the grids. This is fine in low-resolution maps (~0.5 degree), but unrealistic at high-resolution maps, where waste is emitted at one point, rather than throughout the grids. It would therefore be better to provide location-specific information on WWTPs")
      # }
      # 
      # if(wwtp_available==3 && reso>0.1){
      #   print("Unless WWTP location data are available for the full study area, the combination of resolution and use of WWTP location data is unrealistic, because at present all emissions from people connected to sewers will go to these WWTPs")
      # }
      # 
      # #Are you interested in emissions to water only, or also river concentrations?
      # interest_concentrations<<-scenario$interest_concentrations
      # if(identical(interest_concentrations,FALSE)){
      #   print("Concentrations will not be calculated. Hydrology is only used to estimate runoff of faeces on the land")
      # }
      # 
      # Sys.time()
      # # in case you do not want to run the human_emissions script all at once, you can stop here with running
      # # the model line by line and move to the human emissions script and run that script line by line.
      # # contains grid for land, water and emissions data.frame
      # human_emissions <- human.emissions.model.run(scenario,pathogen,isoraster)
      # print("finished human emissions")
      # Sys.time()
      # 
      # if(hydrology_available){
      #   # TODO: write raster here or skip, because it was not used?
      #   if(interest_concentrations){
      #     source(file.path(working.path.script,"calling_concentrations.R"))
      #     print("finished concentrations")
      #   }
      # }
    }
    print(i)
  }
  
  else{
    warning("No runs found in configuration")
  }
  
}





