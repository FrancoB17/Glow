#This code is the main code to run the GloWPa model. The model can be run for various waterborne pathogens
#and and various spatial scales and resolutions. This code is the basis for the K2P mapping tool, among others.
#This code was written by Nynke Hofstra in May 2019

#still to do: 
# add possible multiplying variables for all other variables than the ones already done
# test the makeisogrids part
# do we need to perform some checks everywhere to make sure the data are in the right format? We probably do. To be done.

rm(list=ls(all=TRUE))
wd<-"C:/Users/hofst023/OneDrive - WageningenUR/Oude D schijf/Onderzoek/K2P/GloWPa model/temporary/"
model_path<<-"C:/Users/hofst023/OneDrive - WageningenUR/Oude D schijf/Onderzoek/K2P/GloWPa model/"
wd<<-paste0(model_path,"Model output/Global/")
working.path.in<<-paste0(model_path,"Model input/Global/")
working.path.in.pathogens<<-paste0(model_path,"Model input/")
working.path.script<<-paste0(model_path,"Model scripts/")
working.path.out<<-paste0(model_path,"Model output/Global/")

#In case you want to set up scenarios, you can simply change the file overall_inputs.csv 
#by adding more lines.

#############AFTER THIS LINE THERE IS NO NEED TO CHANGE ANYTHING IN THIS SCRIPT OR OTHER SCRIPTS#####################
setwd(wd)

library(sp)
library(rgeos)
library(maptools)
library(rworldmap)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rgdal)
#read in overall input file
overall_inputs<-read.csv(paste0(working.path.in,"overall_inputs.csv"),stringsAsFactors=FALSE)
runs<<-length(overall_inputs$run) #runs is the number of scenarios to be run

#now run the model. First the human emissions model, then the calling_concentrations part that estimates the concentrations
mons <<- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for (i in 1:runs){
  
  run<-overall_inputs$run
#Identify the area. Here the grids within the area are assigned to the code of the area. Here a file is read in
#that has either the polygons of areas or is a gridded file. The polygon file should always be transformed to the
#set resolution.
#makeisogrids.R needs to be run only the first time when you have polygon data. Afterwards, the isoreadformat can be
#set to 2, as the isogrids file has been produced already.
  reso<<-overall_inputs$resolution   #this resolution could be 0.5, 0.08333 or 0.008333 degree latitude x longitude (this corresponds to roughly 50x50, 10x10 and 1x1 km2)
#  isoreadformat<<-2  #this is a switch to select in which format the iso information is read in. 1 stands for polygons, 2 stands for gridded
#  if(isoreadformat==1 && isTRUE(first_time_area)){ #making isogrids is only required when this file needs to be made from polygons
#    layer<<-"" #this is the name of the layer with the polygon file. It is supposed to be a shape file
#    #in case the shape file is not in a standard lat-lon projection (WGS84), they need to also be transformed
#    #that does currently not happen in the makeisogrids program.
#    source(paste0(working.path.script,"makeisogrids.R"))
#    print("finished makeisogrid")
#  }
  

  #The isoraster needs to be prepared before running the model. This raster is a file that links 
  #an area on the map to the district number it is located in.
  isoraster<<-raster(paste0(working.path.in,overall_inputs$isoraster_filename)) #now read the isogrids
  area_extent<<-extent(isoraster) #this extent covers the selected area. this extent can later be used to crop data (e.g. population)

  #check that the correct resolution isoraster data have been read in

  if((reso-xres(isoraster))>1e-5 || (reso-yres(isoraster))>1e-5){
    print("ERROR: isoraster file has incorrect resolution!")
    quit
  
  }
  #livestock switch - for now switched off. Later on the model can be adapted to include also livestock

  livestock_switch=overall_inputs$livestock_switch

  #select the pathogen:
  pathogen<<-overall_inputs$pathogen #select pathogen. Options: rotavirus, cryptosporidium

  #later on we may want to set an option that several pathogens can be selected at the same time -> in that case the loop below should be run multiple times with different input variables (easiest). This won't be the quickest option. It would be much quicker to run them simultaneiously, as for example the distribution over grids, the hydrology etc will be the same.
  if(pathogen=="cryptosporidium" & identical(livestock_switch,FALSE)){
    print(paste0("Warning: The pathogen ",pathogen," also has a livestock source. Please be aware the current model only simulates the human source"))
  }
  #read in the pathogen specific input data
  pathogen_inputs<<-read.csv(paste0(working.path.in.pathogens,"pathogen_inputs.csv"),stringsAsFactors=FALSE)
  pathogen_row<<-which(pathogen_inputs$name==pathogen)
  if(length(pathogen_row)<1){
    print("Error: pathogen misspelled or not in list")
  }
  #is local - area specific incidence data available?

  incidence_available<<-overall_inputs$incidence_available

  if(isTRUE(incidence_available)){
    pathogen_inputs$incidence_highhdi_over5[pathogen_row]<<-1 #change number - in case you are in a lowHDI area, you can leave this
    pathogen_inputs$incidence_highhdi_under5[pathogen_row]<<-1 #change number - in case you are in a lowHDI area, you can leave this
    pathogen_inputs$incidence_lowhdi_over5[pathogen_row]<<-1 #change number - in case you are in a highHDI area, you can leave this
    pathogen_inputs$incidence_lowhdi_under5[pathogen_row]<<-1 #change number - in case you are in a highHDI area, you can leave this
    print("Constand incidence is used throughout the study area. Only in larger areas HDI is used to distinguish areas with high and low incidence")
  }else{
  
    print("Generic literature values for pathogen incidence will be used. The results are therefore average background emissions and concentrations.")
  }

  #calculate or provide correct variables to use in model for pathogen
  #calculate extretion/shedding rates per person
  ExcretionLowHDI_child<<-pathogen_inputs$incidence_lowhdi_under5[pathogen_row]*pathogen_inputs$shedding_pp_pd[pathogen_row]*pathogen_inputs$episodelength[pathogen_row]  #episodes per person per year * excretion per day * days per episode
  ExcretionHighHDI_child<<-pathogen_inputs$incidence_highhdi_under5[pathogen_row]*pathogen_inputs$shedding_pp_pd[pathogen_row]*pathogen_inputs$episodelength[pathogen_row]
  ExcretionLowHDI_others<<-pathogen_inputs$incidence_lowhdi_over5[pathogen_row]*pathogen_inputs$shedding_pp_pd[pathogen_row]*pathogen_inputs$episodelength[pathogen_row]
  ExcretionHighHDI_others<<-pathogen_inputs$incidence_highhdi_over5[pathogen_row]*pathogen_inputs$shedding_pp_pd[pathogen_row]*pathogen_inputs$episodelength[pathogen_row]
  #is gridded hydrological data available? Required are discharge, runoff, water level or depth, flow direction, residence time of water in each grid, and possibly water temperature and shortwave radiation to be consistent
  hydrology_available<<-overall_inputs$hydrology_available
  if(identical(hydrology_available,FALSE) & isTRUE(overall_inputs$interest_concentrations)){
    #run the Wflow model in python - don't know how yet
    print("WARNING: Only emissions can be estimated, no concentations yet - this will change once we have the Wflow model running")
  }
  #In some cases the resolution of the hydrological files does not meet the resolution of the isoraster and
  #other files, such as the population density. In those cases we can simulate emissions at the set resolution
  #and concentrations at a lower resolution. Already here we set up a new isoraster file that can be used throughout
  #the model for the concentrations. All files required for the hydrology should be in the same resolution and
  #will be fit to the extent of the isoraster developed here.
  print(pathogen)
  if(isTRUE(hydrology_available)){
    discharge_files<<- list.files(path=(paste0(working.path.in,"discharge")))
    discharge<<-raster(paste0(working.path.in,"discharge/",discharge_files[1]))
    discharge<<-crop(discharge,extent(isoraster))
    isoraster_hydro<<-crop(isoraster,extent(discharge))
    res_multiplier<<-xres(discharge)/xres(isoraster)
    if(res_multiplier>1){
      isoraster_hydro<<-aggregate(isoraster_hydro,fact=res_multiplier,fun=modal,na.rm=TRUE)
    }else if (res_multiplier<1){
      print("ERROR: THE RESOLUTION OF THE HYDROLOGY DATA IS LOWER THAN THE SET RESOLUTION AND THE MODEL HAS NOT BEEN SET UP TO CORRECT FOR THIS!")
    }
  }

  #is location-specific WWTP data available?
  wwtp_available<<-overall_inputs$wwtp_available
  if(wwtp_available==3){
    print("Emissions from the population connected to a sewer will be entered in a single grid (also the sewage not treated). Leaky pipes are therefore missed. This option is only realistic when for the full area covered has data on WWTP location")
  }else if(wwtp_available==2){
    print("The emissions from sewers will be spread over the grids. This is fine in low-resolution maps (~0.5 degree), but unrealistic at high-resolution maps, where waste is emitted at one point, rather than throughout the grids. It would therefore be better to provide location-specific information on WWTPs")
  }

  if(wwtp_available==3 && reso>0.1){
    print("Unless WWTP location data are available for the full study area, the combination of resolution and use of WWTP location data is unrealistic, because at present all emissions from people connected to sewers will go to these WWTPs")
  }

  #Are you interested in emissions to water only, or also river concentrations?
  interest_concentrations<<-overall_inputs$interest_concentrations
  if(identical(interest_concentrations,FALSE)){
    print("Concentrations will not be calculated. Hydrology is only used to estimate runoff of faeces on the land")
  }
  #create results dataframe ##I don't think this currently works. Is from Lucie's model still.
  run_number<<-c(1:runs)
  results<<-data.frame(run_number)
  results$conc_q5<-NA
  results$conc_q25<-NA
  results$conc_q50<-NA
  results$conc_q75<-NA
  results$conc_q95<-NA
  results$conc_mean<-NA
  results$load_q5<-NA
  results$load_q25<-NA
  results$load_q50<-NA
  results$load_q75<-NA
  results$load_q95<-NA
  results$load_mean<-NA
  results$f_diffuse_jan<-NA
  results$f_diffuse_feb<-NA
  results$f_diffuse_mar<-NA
  results$f_diffuse_apr<-NA
  results$f_diffuse_may<-NA
  results$f_diffuse_jun<-NA
  results$f_diffuse_jul<-NA
  results$f_diffuse_aug<-NA
  results$f_diffuse_sep<-NA
  results$f_diffuse_oct<-NA
  results$f_diffuse_nov<-NA
  results$f_diffuse_dec<-NA
  results$load_diffuse_jan<-NA
  results$load_diffuse_feb<-NA
  results$load_diffuse_mar<-NA
  results$load_diffuse_apr<-NA
  results$load_diffuse_may<-NA
  results$load_diffuse_jun<-NA
  results$load_diffuse_jul<-NA
  results$load_diffuse_aug<-NA
  results$load_diffuse_sep<-NA
  results$load_diffuse_oct<-NA
  results$load_diffuse_nov<-NA
  results$load_diffuse_dec<-NA
  results$load_point_jan<-NA
  results$load_point_feb<-NA
  results$load_point_mar<-NA
  results$load_point_apr<-NA
  results$load_point_may<-NA
  results$load_point_jun<-NA
  results$load_point_jul<-NA
  results$load_point_aug<-NA
  results$load_point_sep<-NA
  results$load_point_oct<-NA
  results$load_point_nov<-NA
  results$load_point_dec<-NA
  results$routed_f<-NA
  results$routed_oo<-NA
  
  Sys.time()
  source(paste0(working.path.script,"Human_emissions.R"))
  print("finished human emissions")
  Sys.time()

  if(isTRUE(hydrology_available) && isTRUE(interest_concentrations)){
    source(paste0(working.path.script,"calling_concentrations.R"))
    print("finished concentrations")
  }
  print(i)

}

