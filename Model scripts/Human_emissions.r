# human emissions model developed by Lucie and Nynke
# updated to run from GloWPa_model.R in May 2019 by Nynke
# option to include location of WWTP has been added
# emissions to land have been separated and onsite sytems have been included


# Read in the human data input file and the WWTP data file ----------------
# For the current version of the model (and I think we should keep that in addition to the
# merged version), the human data file has 4 sanitation categories. Matt's version of the 
# model has 15 sanitation categories, which means the .csv file for the merged tool is
# different from the .csv file for the current tool.

#open .csv file with sub-area specific data
#requirements for / consequences of the HumanData file:
#con+dif+dir+onsite should be 1 for urban and 1 for rural
#onsite_treated+onsite_water+onsite_land+onsite_flush+onsite_land should be 1 for urban and 1 for rural
#treatm_1+treatm2+treatm3+treatm_4+treatm_ponds+treatm_no should be 1

#In case wwtpdata_available==TRUE: only in the districts with WWTPs the treatment fractions are used; this fraction is then applied for all waste treated in that district.
library(dplyr)

human.emissions.model.run <- function(scenario,pathogen,isoraster){
  
  #calculate or provide correct variables to use in model for pathogen
  #calculate extretion/shedding rates per person
  ExcretionLowHDI_child<<-pathogen$incidence_lowhdi_under5*pathogen$shedding_pp_pd*pathogen$episodelength  #episodes per person per year * excretion per day * days per episode
  excretion_child<<-pathogen$incidence_highhdi_under5*pathogen$shedding_pp_pd*pathogen$episodelength
  ExcretionLowHDI_others<<-pathogen$incidence_lowhdi_over5*pathogen$shedding_pp_pd*pathogen$episodelength
  excretion_others<<-pathogen$incidence_highhdi_over5*pathogen$shedding_pp_pd*pathogen$episodelength
  
  area_extent <-extent(isoraster) #this extent covers the selected area. this extent can later be used to crop data (e.g. population)
  # READ HUMAN & WWTP
  HumanData<-read.csv(paste0(working.path.in,scenario$HumanData_filename,".csv"),sep=",",header=TRUE,col.names=c("countryname","iso","hdi","pop_totalx1000","f_urb","f_rur","f_under5","f_over5","con_urb", "con_rur","dir_urb","dir_rur","dif_urb","dif_rur","onsite_urb","onsite_rur","onsite_urb_treated","onsite_rur_treated","onsite_urb_water","onsite_rur_water","onsite_urb_land","onsite_rur_land","onsite_urb_flush","onsite_rur_flush","onsite_urb_remain","onsite_rur_remain", "treatm_1","treatm_2","treatm_3","treatm_4","treatm_ponds","treatm_no","storage_empty_urb","storage_empty_rur","storage_flush_urb","storage_flush_rur"),numerals="warn.loss") #paste0(overall_inputs$HumanData_filename[i],".csv"
  HumanData<-HumanData[order(HumanData$iso),]
  HumanData$pop_totalx1000<-HumanData$pop_totalx1000*scenario$population_multiplication
  
  #open .csv file with WWTP data
  # if 3 then location of WWTP known
  # if 2 then treatment, but location unknown
  # if 1 then no treatment
  if(scenario$wwtp_available==3){
    WWTP_inputs<-read.csv(paste0(working.path.in,scenario$WWTPData_filename[i],".csv"))  #file has district, lat, lon, capacity, system categories. Would be good to add treatment type (if available) and reduction (if available) -> these data should come from Matt's model later on. For now just average removal has been used.
    WWTP_inputs$treatment_type<-as.character(WWTP_inputs$treatment_type)
    WWTP_inputs$subregion<-as.character(WWTP_inputs$subregion)
  }
  
  if(scenario$wwtp_available==1){
    HumanData$treatm_1[]<-0
    HumanData$treatm_2[]<-0
    HumanData$treatm_3[]<-0
    HumanData$treatm_4[]<-0
    HumanData$treatm_ponds[]<-0
    HumanData$treatm_no[]<-1
  }
  
  # READ POPULATION DATA
  # Read in population data and data correction for population in csv file --------
  # Currently we are using landscan. I am considering to change to popgrid, as this has more options.
  # I would prefer to change to a datafile that we can also use for future scenarios. Not sure if that exists.
  
  # read in gridded urban and rural population data
  
  popurban_grid<-raster(file.path(working.path.in,scenario$population_urban_filename))
  poprural_grid<-raster(file.path(working.path.in,scenario$population_rural_filename))
  
  # check that the correct resolution population data have been read in
  if((reso-xres(popurban_grid))>1e-5 || (reso-yres(popurban_grid))>1e5 || (reso-xres(poprural_grid))>1e-5 || (reso-yres(poprural_grid))>1e-5){
    stop("ERROR: population file(s) are incorrect resolution!")
  }
  
  # crop the population data to the extent of the isoraster
  popurban_grid <- crop(popurban_grid,area_extent)
  poprural_grid <- crop(poprural_grid,area_extent)
  
  # calc population per area and area type
  HumanData <- calc.pop.per.area.type(popurban_grid,poprural_grid,isoraster, HumanData)
  # calc corrected population
  pop_corrected <- correct.population(popurban_grid,popurban_grid,isoraster,HumanData)
  
  poprural_grid <- pop_corrected$rural
  
  # CALCULATION OF EMISSIONS PER SANITION TYPE AND SUBAREA
  # Be aware: in case we have WWTP location data, the reduction in those has not yet been added
  # to the emissions. That is why after this section the point dataframe cannot yet be saved as file.
  emissions <- get.emissions(scenario,pathogen,HumanData)
  emissions <- calc.pop.emissions(emissions,HumanData)
  # clean na values
  emissions <- remove.na.emissions(emissions)
  emissions <- calc.land.emissions(emissions)
  
  #initiate pathogen_treatment
  pathogen_treatment<-isoraster
  pathogen_treatment[]<-0
  
  if(scenario$wwtp_available ==3){
    #find the subarea in which the grid is located
    WWTP_inputs$subarea <- raster::extract(isoraster,matrix(data=c(WWTP_inputs$Lon,WWTP_inputs$Lat),nrow=length(WWTP_inputs$Lon),ncol=2))
    WWTP_inputs <- calc.wwtp.emissions(WWTP_inputs,pathogen)
    
    emissions <- calc.wttp.emissions.subarea(emissions, WWTP_inputs, HumanData)
    pathogen_treatment[cellFromXY(isoraster,matrix(data=c(WWTP_inputs$Lon,WWTP_inputs$Lat),nrow=length(WWTP_inputs$Lon),ncol=2))]<-WWTP_inputs$emissions_per_WWTP
  }
  pathogen_urban_water_grid <- calc.emissions.grid(isoraster, HumanData$iso, emissions$pathogen_urb_waterforgrid_pp,popurban_grid)
  pathogen_rural_water_grid <- calc.emissions.grid(isoraster, HumanData$iso, emissions$pathogen_rur_waterforgrid_pp, poprural_grid)
  # TODO: check with Nynke if this is allowed when no wwtp
  pathogen_water_grid <- pathogen_urban_water_grid+pathogen_rural_water_grid+pathogen_treatment
  
  pathogen_urban_land_grid <- calc.emissions.grid(isoraster,HumanData$iso, emissions$pathogen_urb_landforgrid_pp,popurban_grid)
  pathogen_rural_land_grid <- calc.emissions.grid(isoraster,HumanData$iso, emissions$pathogen_rur_landforgrid_pp,poprural_grid)
  pathogen_land_grid <- pathogen_urban_land_grid+pathogen_rural_land_grid
  
  #make sure the output file puts the connected fraction in the right district, applying treatment also when
  #wwtpdata is available.
  # TODO: check if this can be moved up
  if(wwtp_available==3){
    emissions$pathogen_urb_con<-emissions$pathogen_urb_WWTP_sewer
    emissions$pathogen_rur_con<-emissions$pathogen_rur_WWTP_sewer
    emissions$pathogen_urb_onsite_treated<-emissions$pathogen_urb_WWTP_onsite
    emissions$pathogen_rur_onsite_treated<-emissions$pathogen_rur_WWTP_onsite
  }
  return(list(land=pathogen_land_grid, water=pathogen_water_grid),emissions=emissions)
}

calc.pop.per.area.type <- function(pop_urban_grid,pop_rulal_grid,isoraster,HumanData){
  # Now the gridded population data need to be corrected to match the HumanData populationdata when grids are summed
  # first make arrays with the summed urban and rural populations over the grids in the iso codes. This bit was
  # written with help of Benjamin Brede - thanks to him it sped up a lot, compared to the loop I used first.
  all_data <- data.frame(Urban = values(pop_urban_grid),Rural = values(pop_rulal_grid),ISO = values(isoraster))
  summed <- all_data %>%
    filter(ISO %in% HumanData$iso) %>% # restrict to only ISOs in HumanData_iso
    group_by(ISO) %>% # do the following per ISO code
    summarise(UrbanTotal = sum(Urban, na.rm = TRUE),
              RuralTotal = sum(Rural, na.rm = TRUE)) %>%
    ungroup()
  HumanData <- HumanData[(HumanData$iso %in% summed$ISO), ]
  
  HumanData$popurb<-NA
  HumanData$poprur<-NA
  
  for (j in 1:length(HumanData$iso)){
    q<-which(summed$ISO==HumanData$iso[j])
    if(length(q)>0){
      HumanData$popurb[q]<-summed$UrbanTotal[q]
      HumanData$poprur[q]<-summed$RuralTotal[q]
    }
  }

  return(HumanData)
}

correct.population <- function(pop_urban_grid,pop_rulal_grid,isoraster,HumanData){
  # Then the population grids need to be corrected for the difference between the summed gridded population and
  # the HumanData population (the latter is supposed to have the most correct values.)
  urbanpop2010<-HumanData$f_urb*HumanData$pop_total
  ruralpop2010<-(1-HumanData$f_urb)*HumanData$pop_total
  
  dif_urban<-urbanpop2010/HumanData$popurb
  dif_rural<-ruralpop2010/HumanData$poprur
  
  dif_urban<-data.frame(iso=HumanData$iso,value=dif_urban)
  dif_urban_raster<-subs(isoraster, dif_urban, subsWithNA=T)
  
  dif_rural<-data.frame(iso=HumanData$iso,value=dif_rural)
  dif_rural_raster<-subs(isoraster, dif_rural, subsWithNA=T)
  
  popurban_grid<-pop_urban_grid*dif_urban_raster
  poprural_grid<-pop_rulal_grid*dif_rural_raster
  
  return(list(urban=popurban_grid,rural=poprural_grid))
}

#' Emissions per sanitation type
#' Calculation of emissions per sanitation type and subarea.
#' @details Be aware: in case we have WWTP location data, the reduction in those has not yet been added to the emissions. 
#' That is why after this section the point dataframe cannot yet be saved as file.
#' @param scenario {data.frame} - scenario input data as data.frame with 1 row and many observations
#' @param pathogen {data.frame} - pathogen input data as data.frame with 1 row and many observations
#' @param HumanData {data.frame} - human data input data containing geograhical properties (column) per area (row)
#'
#' @return {data.frame} - containing calculated emissions per saniation type and area 
#' @export
#'
#' @examples
get.emissions <- function(scenario,pathogen,HumanData){
  
  #fill all the added columns
  HumanData$removalfraction <- 
    1-(HumanData$treatm_1*pathogen$RemovalPrimary
       +HumanData$treatm_2*pathogen$RemovalSecondary
       +HumanData$treatm_3*pathogen$RemovalTertiary
       +HumanData$treatm_4*pathogen$RemovalQuaternary
       +HumanData$treatm_ponds*pathogen$RemovalPonds)
  HumanData$pop_urb_num <- HumanData$pop_totalx1000*HumanData$f_urb
  HumanData$pop_rur_num <- HumanData$pop_totalx1000*HumanData$f_rur
  HumanData$pop_urb_under5 <- HumanData$pop_urb_num*HumanData$f_under5
  HumanData$pop_rur_under5 <- HumanData$pop_rur_num*HumanData$f_under5
  HumanData$pop_urb_over5 <- HumanData$pop_urb_num*HumanData$f_over5
  HumanData$pop_rur_over5 <- HumanData$pop_rur_num*HumanData$f_over5
  
  #make dataframe to fill with urban and rural pathogen emissions
  point <- array(dim=c(length(HumanData$iso),34))
  colnames(point) <- c("countryname","iso","pathogen_urb_con","pathogen_rur_con","pathogen_urb_dir","pathogen_rur_dir","pathogen_urb_dif","pathogen_rur_dif","pathogen_urb_onsite_treated","pathogen_rur_onsite_treated","pathogen_urb_onsite_water","pathogen_rur_onsite_water","pathogen_urb_onsite_land","pathogen_rur_onsite_land","pathogen_urb_onsite_flush","pathogen_rur_onsite_flush","pathogen_urb_WWTP_sewer","pathogen_rur_WWTP_sewer","pathogen_urb_WWTP_onsite","pathogen_rur_WWTP_onsite","pathogen_urb_conforgrid","pathogen_rur_conforgrid","pathogen_urb_waterforgrid","pathogen_rur_waterforgrid","pathogen_urb_landforgrid","pathogen_rur_landforgrid","pathogen_urb_waterforgrid_pp","pathogen_rur_waterforgrid_pp","pathogen_urb_landforgrid_pp","pathogen_rur_landforgrid_pp","pathogen_urb_fconsewer","pathogen_rur_fconsewer","pathogen_urb_flandonsite","pathogen_rur_flandonsite")
  point <- as.data.frame(point)
  
  #fill dataframe
  point$countryname <- HumanData$countryname
  point$iso < -HumanData$iso
  
  #find developed and developing countries & identify excretion categories
  #a = developing, b=developed
  #identify countries that fall into the different excretion categories
  HDIboundary <- scenario$HDIboundary
  a <- which(HumanData$hdi <= HDIboundary) #low HDI
  b <- which(HumanData$hdi > HDIboundary)  #high HDI
  
  NappyCor<-scenario$NappyCor
  UrbanDirect2Water <- scenario$UrbanDirect2Water
  RuralDirect2Water <- scenario$RuralDirect2Water
  UrbanDiffuse2Water <- scenario$UrbanDiffuse2Water
  RuralDiffuse2Water <- scenario$RuralDiffuse2Water
  UrbanOnsite2Water <- scenario$UrbanOnsite2Water
  RuralOnsite2Water <- scenario$RuralOnsite2Water
  Kpit<-pathogen$Kpit
  
  #this function calculates the decay in the onsite systems
  f2<-function(t){
    V<-exp(-Kpit*t*365)
    return(V)
  }
  
  HumanData$survival_pit_empty_urb<-NA
  HumanData$survival_pit_empty_rur<-NA
  HumanData$survival_pit_flush_urb<-NA
  HumanData$survival_pit_flush_rur<-NA
  
  #for HDI high
  output <- calc.emissions(HumanData,point,b,ExcretionHighHDI_child, ExcretionHighHDI_others)
  HumanData <- output$human
  point<- output$emissions

  #for HDI low
  output <- calc.emissions(HumanData,point,a,ExcretionLowHDI_child, ExcretionHighHDI_others)
  HumanData <- output$human
  point<- output$emissions
  
  return(point)
}

calc.emissions <- function(HumanData, point, subset ,excretion_child, excretion_others){
  if(length(subset>0)){
    for (j in 1:length(HumanData$iso[subset])){ #calculate per subarea
      #calculate decay in onsite systems
      int<-integrate(f2,0,HumanData$storage_empty_urb[subset[j]])
      HumanData$survival_pit_empty_urb[subset[j]]<-int$value/HumanData$storage_empty_urb[subset[j]]
      int<-integrate(f2,0,HumanData$storage_empty_rur[subset[j]])
      HumanData$survival_pit_empty_rur[subset[j]]<-int$value/HumanData$storage_empty_rur[subset[j]]
      
      int1<-integrate(f2,0,HumanData$storage_flush_urb[subset[j]])
      HumanData$survival_pit_flush_urb[subset[j]]<-int1$value/HumanData$storage_flush_urb[subset[j]]
      
      int1<-integrate(f2,0,HumanData$storage_flush_rur[subset[j]])
      HumanData$survival_pit_flush_rur[subset[j]]<-int1$value/HumanData$storage_flush_rur[subset[j]]
      
      #calculate the emissions by the specific population groups
      #in case people are interested in children/adults, these number can be split into <5 and >5, but this complicates the model
      point$pathogen_urb_con[subset[j]]<-HumanData$con_urb[subset[j]]*(excretion_child*NappyCor*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])
      point$pathogen_rur_con[subset[j]]<-HumanData$con_rur[subset[j]]*(excretion_child*NappyCor*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])
      point$pathogen_urb_dir[subset[j]]<-HumanData$dir_urb[subset[j]]*UrbanDirect2Water*(excretion_child*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])
      point$pathogen_rur_dir[subset[j]]<-HumanData$dir_rur[subset[j]]*RuralDirect2Water*(excretion_child*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])
      point$pathogen_urb_dif[subset[j]]<-HumanData$dif_urb[subset[j]]*UrbanDiffuse2Water*(excretion_child*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])
      point$pathogen_rur_dif[subset[j]]<-HumanData$dif_rur[subset[j]]*RuralDiffuse2Water*(excretion_child*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])
      point$pathogen_urb_onsite_treated[subset[j]]<-HumanData$onsite_urb_treated[subset[j]]*HumanData$survival_pit_empty_urb[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])/HumanData$storage_empty_urb[subset[j]]
      point$pathogen_rur_onsite_treated[subset[j]]<-HumanData$onsite_rur_treated[subset[j]]*HumanData$survival_pit_empty_rur[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])/HumanData$storage_empty_rur[subset[j]]
      point$pathogen_urb_onsite_water[subset[j]]<-HumanData$onsite_urb_water[subset[j]]*HumanData$survival_pit_empty_urb[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])/HumanData$storage_empty_urb[subset[j]]
      point$pathogen_rur_onsite_water[subset[j]]<-HumanData$onsite_rur_water[subset[j]]*HumanData$survival_pit_empty_rur[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])/HumanData$storage_empty_rur[subset[j]]
      point$pathogen_urb_onsite_land[subset[j]]<-HumanData$onsite_urb_land[subset[j]]*HumanData$survival_pit_empty_urb[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])/HumanData$storage_empty_urb[subset[j]]
      point$pathogen_rur_onsite_land[subset[j]]<-HumanData$onsite_rur_land[subset[j]]*HumanData$survival_pit_empty_rur[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])/HumanData$storage_empty_rur[subset[j]]
      point$pathogen_urb_onsite_flush[subset[j]]<-HumanData$onsite_urb_flush[subset[j]]*HumanData$survival_pit_flush_urb[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_urb_under5[subset[j]]+excretion_others*HumanData$pop_urb_over5[subset[j]])/HumanData$storage_flush_urb[subset[j]]
      point$pathogen_rur_onsite_flush[subset[j]]<-HumanData$onsite_rur_flush[subset[j]]*HumanData$survival_pit_flush_rur[subset[j]]*UrbanOnsite2Water*(excretion_child*NappyCor*HumanData$pop_rur_under5[subset[j]]+excretion_others*HumanData$pop_rur_over5[subset[j]])/HumanData$storage_flush_rur[subset[j]]
      
      if(scenario$wwtp_available==3){
        
        #In this case all emissions from the connected population are not yet included in the emissions to water. These are added to the grids in the next step. Treatment has not been applied yet.
        point$pathogen_urb_conforgrid[subset[j]]<-point$pathogen_urb_con[subset[j]]+point$pathogen_urb_onsite_treated[subset[j]]
        point$pathogen_rur_conforgrid[subset[j]]<-point$pathogen_rur_con[subset[j]]+point$pathogen_rur_onsite_treated[subset[j]]
        point$pathogen_urb_waterforgrid[subset[j]]<-point$pathogen_urb_dir[subset[j]]+point$pathogen_urb_onsite_water[subset[j]]+point$pathogen_urb_onsite_flush[subset[j]]
        point$pathogen_rur_waterforgrid[subset[j]]<-point$pathogen_rur_dir[subset[j]]+point$pathogen_rur_onsite_water[subset[j]]+point$pathogen_rur_onsite_flush[subset[j]]
        point$pathogen_urb_landforgrid[subset[j]]<-point$pathogen_urb_dif[subset[j]]+point$pathogen_urb_onsite_land[subset[j]]
        point$pathogen_rur_landforgrid[subset[j]]<-point$pathogen_rur_dif[subset[j]]+point$pathogen_rur_onsite_land[subset[j]]
        
      } else{
        
        #In this case all emissions from the connected population go to the water after treatment.
        point$pathogen_urb_conforgrid[subset[j]]<-NA
        point$pathogen_rur_conforgrid[subset[j]]<-NA
        point$pathogen_urb_waterforgrid[subset[j]]<-point$pathogen_urb_con[subset[j]]*HumanData$removalfraction[subset[j]]+point$pathogen_urb_dir[subset[j]]+point$pathogen_urb_onsite_treated[subset[j]]*HumanData$removalfraction[subset[j]]+point$pathogen_urb_onsite_water[subset[j]]+point$pathogen_urb_onsite_flush[subset[j]]
        point$pathogen_rur_waterforgrid[subset[j]]<-point$pathogen_rur_con[subset[j]]*HumanData$removalfraction[subset[j]]+point$pathogen_rur_dir[subset[j]]+point$pathogen_rur_onsite_treated[subset[j]]*HumanData$removalfraction[subset[j]]+point$pathogen_rur_onsite_water[subset[j]]+point$pathogen_rur_onsite_flush[subset[j]]
        point$pathogen_urb_landforgrid[subset[j]]<-point$pathogen_urb_dif[subset[j]]+point$pathogen_urb_onsite_land[subset[j]]
        point$pathogen_rur_landforgrid[subset[j]]<-point$pathogen_rur_dif[subset[j]]+point$pathogen_rur_onsite_land[subset[j]]
        
      }
    }
  }

  
  return(list(human=HumanData, emissions=emissions))
}

calc.pop.emissions <- function(emissions,HumanData){
  
  for (j in 1:length(HumanData$iso)){
    
    emissions$pathogen_urb_waterforgrid_pp[j]<-emissions$pathogen_urb_waterforgrid[j]/HumanData$pop_urb_num[j]
    emissions$pathogen_rur_waterforgrid_pp[j]<-emissions$pathogen_rur_waterforgrid[j]/HumanData$pop_rur_num[j]
    
    emissions$pathogen_urb_landforgrid_pp[j]<-emissions$pathogen_urb_landforgrid[j]/HumanData$pop_urb_num[j]
    emissions$pathogen_rur_landforgrid_pp[j]<-emissions$pathogen_rur_landforgrid[j]/HumanData$pop_rur_num[j]
    
  }
  return(emissions)
}

remove.na.emissions <- function(emissions){
  #remove NA from the point dataframe to prevent problems later on.
  a<-which(is.na(emissions$pathogen_urb_conforgrid))
  emissions$pathogen_urb_conforgrid[a]<-0
  a<-which(is.na(emissions$pathogen_rur_conforgrid))
  emissions$pathogen_rur_conforgrid[a]<-0
  a<-which(is.na(emissions$pathogen_urb_waterforgrid))
  emissions$pathogen_urb_waterforgrid[a]<-0
  a<-which(is.na(emissions$pathogen_rur_waterforgrid))
  emissions$pathogen_rur_waterforgrid[a]<-0
  a<-which(is.na(emissions$pathogen_urb_landforgrid))
  emissions$pathogen_urb_landforgrid[a]<-0
  a<-which(is.na(emissions$pathogen_rur_landforgrid))
  emissions$pathogen_rur_landforgrid[a]<-0
  a<-which(is.na(emissions$pathogen_urb_con))
  emissions$pathogen_urb_con[a]<-0
  a<-which(is.na(emissions$pathogen_rur_con))
  emissions$pathogen_rur_con[a]<-0
  a<-which(is.na(emissions$pathogen_urb_onsite_land))
  emissions$pathogen_urb_onsite_land[a]<-0
  a<-which(is.na(emissions$pathogen_rur_onsite_land))
  emissions$pathogen_rur_onsite_land[a]<-0
  a<-which(is.na(emissions$pathogen_urb_dif))
  emissions$pathogen_urb_dif[a]<-0
  a<-which(is.na(emissions$pathogen_rur_dif))
  emissions$pathogen_rur_dif[a]<-0
  return(emissions)
}

#' Emissions to land
#' Estimate fraction of connected and land emissions for the different systems to later do source tracking
#' @param emissions
#' @return
#' @export
#'
#' @examples
calc.land.emissions <- function(emissions){
  emissions$pathogen_urb_fconsewer<-0
  emissions$pathogen_rur_fconsewer<-0
  emissions$pathogen_urb_flandonsite<-0
  emissions$pathogen_rur_flandonsite<-0
  emissions$pathogen_urb_flanddiffuse<-0
  emissions$pathogen_rur_flanddiffuse<-0
  
  a<-which(emissions$pathogen_urb_conforgrid>0)
  b<-which(emissions$pathogen_rur_conforgrid>0)
  d<-which(emissions$pathogen_urb_landforgrid+emissions$pathogen_rur_landforgrid>0)
  
  emissions$pathogen_urb_fconsewer[a]<-emissions$pathogen_urb_con[a]/emissions$pathogen_urb_conforgrid[a]
  emissions$pathogen_rur_fconsewer[b]<-emissions$pathogen_rur_con[b]/emissions$pathogen_rur_conforgrid[b]
  emissions$pathogen_urb_flandonsite[d]<-emissions$pathogen_urb_onsite_land[d]/(emissions$pathogen_urb_landforgrid[d]+emissions$pathogen_rur_landforgrid[d])
  emissions$pathogen_rur_flandonsite[d]<-emissions$pathogen_rur_onsite_land[d]/(emissions$pathogen_urb_landforgrid[d]+emissions$pathogen_rur_landforgrid[d])
  emissions$pathogen_urb_flanddiffuse[d]<-emissions$pathogen_urb_dif[d]/(emissions$pathogen_urb_landforgrid[d]+emissions$pathogen_rur_landforgrid[d])
  emissions$pathogen_rur_flanddiffuse[d]<-emissions$pathogen_rur_dif[d]/(emissions$pathogen_urb_landforgrid[d]+emissions$pathogen_rur_landforgrid[d])
  return(emissions)
  #this means: fcononsite=1-fconsewer and flanddirect=1-flandonsite
  
  #previously we were saving the results to the human inputs here. We cannot do that now, because we cannot include the
  #runoff of the land. Therefore this is moved to the next section, after the runoff.
  #write.csv(point, file=paste0(working.path.out,"HumanEmissionsCalculated_",pathogen,"_",i,".csv"))
}

calc.wwtp.emissions <- function(WWTP_inputs,pathogen){
  #find the HDI for this subarea
  #removal rate of the WWTP is set to the removal rate of the district, only when this rate is not specified in the input file
  #MAKE SURE THAT THE REMOVAL RATE FROM THE SKETCHER IS IN FRACTION RATHER THAN LOG UNIT! IF LOG REMOVAL, CORRECT BY fred<-1-10^(-logremoval)
  
  for(j in 1:length(WWTP_inputs$subarea)){
    #    WWTP_inputs$hdi[j]<-HumanData$hdi[which(HumanData$iso==WWTP_inputs$subarea[j])]
    #    if(is.na(WWTP_inputs$removalfraction_sewer[j])){ #if the removal fraction has not been specified, then use the data from the district, otherwise the data is Matt's data, or estimated with the sketcher tool
    #      WWTP_inputs$removalfraction_sewer[j]<-HumanData$removalfraction[which(HumanData$iso==WWTP_inputs$subarea[j])]
    #    }
    
    if(identical(WWTP_inputs$treatment_type[j],"primary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalPrimary
      WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalPrimary
    }else if(identical(WWTP_inputs$treatment_type[j],"secondary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalSecondary
      WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalSecondary
    }else if(identical(WWTP_inputs$treatment_type[j],"tertiary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalTertiary
      WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalTertiary
    }else if(identical(WWTP_inputs$treatment_type[j],"quaternary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalQuaternary
      WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalQuaternary
    }else if(identical(WWTP_inputs$treatment_type[j],"ponds")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalPonds
      WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalPonds
    }else if(identical(WWTP_inputs$treatment_type[j],"not treated")){
      WWTP_inputs$removalfraction_sewer[j]<-0
      WWTP_inputs$removalfraction_onsite[j]<-0
    }
    
    #    if(is.na(WWTP_inputs$removalfraction_onsite[j])){
    #      WWTP_inputs$removalfraction_onsite[j]<-HumanData$removalfraction[which(HumanData$iso==WWTP_inputs$subarea[j])]
    #    }
    
  }
  
  #estimate the fraction of people connected per capacity unit
  capacity <- sum(WWTP_inputs$Capacity,na.rm=TRUE)  #how to deal with capacity is NA -> WWTP non existing?
  WWTP_inputs$fcapacity <- WWTP_inputs$Capacity/capacity
  
  #calculate the emissions per WWTP
  #the removal file for the WWTP is used if specified from the Sketcher tool, or the removal rate from the
  #literature is used (specified in for loop above)
  WWTP_inputs$emissions_per_WWTP_sewer_urb <- WWTP_inputs$fcapacity*(sum((emissions$pathogen_urb_conforgrid*emissions$pathogen_urb_fconsewer),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_sewer)
  WWTP_inputs$emissions_per_WWTP_sewer_rur <- WWTP_inputs$fcapacity*(sum((emissions$pathogen_rur_conforgrid*emissions$pathogen_rur_fconsewer),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_sewer)
  WWTP_inputs$emissions_per_WWTP_onsite_urb <- WWTP_inputs$fcapacity*(sum((emissions$pathogen_urb_conforgrid*(1-emissions$pathogen_urb_fconsewer)),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_onsite)
  WWTP_inputs$emissions_per_WWTP_onsite_rur <- WWTP_inputs$fcapacity*(sum((emissions$pathogen_rur_conforgrid*(1-emissions$pathogen_rur_fconsewer)),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_onsite)
  WWTP_inputs$emissions_per_WWTP <- WWTP_inputs$emissions_per_WWTP_sewer_urb+WWTP_inputs$emissions_per_WWTP_sewer_rur+WWTP_inputs$emissions_per_WWTP_onsite_urb+WWTP_inputs$emissions_per_WWTP_onsite_rur
  
  print("Warning: currently average treatment coverage and efficiency are used. No link is yet established with the pathogen flow tool")
  return(WWTP_inputs)
}

calc.wttp.emissions.subarea <- function(emissions,WWTP_inputs,HumanData){
  
  WWTP_emissions_per_subarea<-array(dim=c(length(unique(WWTP_inputs$subarea)),5))
  colnames(WWTP_emissions_per_subarea)<-c("subarea","sewer_urb","sewer_rur","onsite_urb","onsite_rur")
  WWTP_emissions_per_subarea<-as.data.frame(WWTP_emissions_per_subarea)
  
  for(j in 1:length(unique(WWTP_inputs$subarea))){
    temp<-which(WWTP_inputs$subarea==unique(WWTP_inputs$subarea)[j])
    WWTP_emissions_per_subarea$subarea[j]<-WWTP_inputs$subarea[temp[1]]
    WWTP_emissions_per_subarea$sewer_urb[j]<-sum(WWTP_inputs$emissions_per_WWTP_sewer_urb[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$sewer_rur[j]<-sum(WWTP_inputs$emissions_per_WWTP_sewer_rur[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$onsite_urb[j]<-sum(WWTP_inputs$emissions_per_WWTP_onsite_urb[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$onsite_rur[j]<-sum(WWTP_inputs$emissions_per_WWTP_onsite_rur[temp],na.rm=TRUE)
  }
  
  emissions$pathogen_urb_WWTP_sewer<-0
  emissions$pathogen_rur_WWTP_sewer<-0
  emissions$pathogen_urb_WWTP_onsite<-0
  emissions$pathogen_rur_WWTP_onsite<-0
  
  for(j in 1:length(WWTP_emissions_per_subarea$subarea)){
    emissions$pathogen_urb_WWTP_sewer[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_urb[j]
    emissions$pathogen_rur_WWTP_sewer[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_rur[j]
    emissions$pathogen_urb_WWTP_onsite[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_urb[j]
    emissions$pathogen_rur_WWTP_onsite[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_rur[j]
  }
  return(emissions)
}

calc.emissions.grid <- function(isoraster,iso,emissions_forgrid_pp,pop_grid){
  pathogen_pp<-data.frame(iso=iso,value=emissions_forgrid_pp)
  pathogen_pp_raster<-subs(isoraster, pathogen_pp , subsWithNA=T)
  pathogen_grid <- pathogen_pp_raster * pop_grid

  temp<-data.frame(bla=NA,value=0)
  pathogen_grid <- subs(pathogen_grid,temp,subsWithNA=F)
  return(pathogen_grid)
}

f <- function(a){
  a$b <- 10
}