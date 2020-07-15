
wwtp.run <- function(emissions,pathogen,popurban_grid,poprural_grid,wwtp_inputs=NULL){
  if(SCENARIO$wwtp_available ==3){
    if(SCENARIO$loadings_module == 1){
      # TODO: move this to human_emissions
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
      
    }
    output <- wwtp.calc.treatment(pathogen,emissions,wwtp_inputs)
    pathogen_treatment <- output$treatment
    emissions <- output$emissions
    
    # calculations wttp for human emissions 
    if(SCENARIO$loadings_module ==1){
      
      # TODO: think about moving this piece of code to human_emissions.R
      emissions$pathogen_urb_con<-emissions$pathogen_urb_WWTP_sewer
      emissions$pathogen_rur_con<-emissions$pathogen_rur_WWTP_sewer
      emissions$pathogen_urb_onsite_treated<-emissions$pathogen_urb_WWTP_onsite
      emissions$pathogen_rur_onsite_treated<-emissions$pathogen_rur_WWTP_onsite
    }
    # calculations wwtp for pathogenflows emissions
    else if(SCENARIO$loadings_module == 2){
      # call wttp part in pathogenflows.R
      emissions <- pathogenflow.wwtp(emissions)
    }
  }
  
  pathogen_urban_water_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_urb_waterforgrid_pp)
  pathogen_urban_water_pp_raster<-subs(ISORASTER, pathogen_urban_water_pp , subsWithNA=T)
  
  pathogen_rural_water_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_rur_waterforgrid_pp)
  pathogen_rural_water_pp_raster<-subs(ISORASTER, pathogen_rural_water_pp , subsWithNA=T)
  
  pathogen_urban_water_grid<-pathogen_urban_water_pp_raster*popurban_grid
  pathogen_rural_water_grid<-pathogen_rural_water_pp_raster*poprural_grid
  
  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_water_grid<-subs(pathogen_urban_water_grid,temp,subsWithNA=F)
  pathogen_rural_water_grid<-subs(pathogen_rural_water_grid,temp,subsWithNA=F)
  
  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_water_grid<-subs(pathogen_urban_water_grid,temp,subsWithNA=F)
  pathogen_rural_water_grid<-subs(pathogen_rural_water_grid,temp,subsWithNA=F)
  
  pathogen_water_grid<-pathogen_urban_water_grid+pathogen_rural_water_grid
  
  #for emissions to land
  pathogen_urban_land_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_urb_landforgrid_pp)
  pathogen_urban_land_pp_raster<-subs(ISORASTER, pathogen_urban_land_pp , subsWithNA=T)
  
  pathogen_rural_land_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_rur_landforgrid_pp)
  pathogen_rural_land_pp_raster<-subs(ISORASTER, pathogen_rural_land_pp , subsWithNA=T)
  
  pathogen_urban_land_grid<-pathogen_urban_land_pp_raster*popurban_grid
  pathogen_rural_land_grid<-pathogen_rural_land_pp_raster*poprural_grid
  
  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)
  
  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)
  
  pathogen_land_grid<-pathogen_urban_land_grid+pathogen_rural_land_grid
  
  return(list(emissions=emissions,grid=list(pathogen_land=pathogen_land_grid,pathogen_water=pathogen_water_grid)))
  
}

wwtp.calc.treatment <- function(pathogen,emissions,wwtp_inputs){
  pathogen_treatment <- ISORASTER
  pathogen_treatment[] <- 0
  wwtp_inputs$subarea<-raster::extract(ISORASTER,matrix(data=c(wwtp_inputs$Lon,wwtp_inputs$Lat),nrow=length(wwtp_inputs$Lon),ncol=2))
  
  for(j in 1:length(wwtp_inputs$subarea)){
    
    if(identical(wwtp_inputs$treatment_type[j],"primary")){
      wwtp_inputs$removalfraction_sewer[j]<-pathogen$RemovalPrimary
      wwtp_inputs$removalfraction_onsite[j]<-pathogen$RemovalPrimary
    }else if(identical(wwtp_inputs$treatment_type[j],"secondary")){
      wwtp_inputs$removalfraction_sewer[j]<-pathogen$RemovalSecondary
      wwtp_inputs$removalfraction_onsite[j]<-pathogen$RemovalSecondary
    }else if(identical(wwtp_inputs$treatment_type[j],"tertiary")){
      wwtp_inputs$removalfraction_sewer[j]<-pathogen$RemovalTertiary
      wwtp_inputs$removalfraction_onsite[j]<-pathogen$RemovalTertiary
    }else if(identical(wwtp_inputs$treatment_type[j],"quaternary")){
      wwtp_inputs$removalfraction_sewer[j]<-pathogen$RemovalQuaternary
      wwtp_inputs$removalfraction_onsite[j]<-pathogen$RemovalQuaternary
    }else if(identical(wwtp_inputs$treatment_type[j],"ponds")){
      wwtp_inputs$removalfraction_sewer[j]<-pathogen$RemovalPonds
      wwtp_inputs$removalfraction_onsite[j]<-pathogen$RemovalPonds
    }else if(identical(wwtp_inputs$treatment_type[j],"not treated")){
      wwtp_inputs$removalfraction_sewer[j]<-0
      wwtp_inputs$removalfraction_onsite[j]<-0
    }
  }
  #estimate the fraction of people connected per capacity unit
  capacity<-sum(wwtp_inputs$Capacity,na.rm=TRUE)  #how to deal with capacity is NA -> WWTP non existing?
  wwtp_inputs$fcapacity<-wwtp_inputs$Capacity/capacity
  
  #calculate the emissions per WWTP
  #the removal file for the WWTP is used if specified from the Sketcher tool, or the removal rate from the
  #literature is used (specified in for loop above)
  wwtp_inputs$emissions_per_WWTP_sewer_urb <- wwtp_inputs$fcapacity*(sum((emissions$pathogen_urb_conforgrid*emissions$pathogen_urb_fconsewer),na.rm=TRUE))*(1-wwtp_inputs$removalfraction_sewer)
  wwtp_inputs$emissions_per_WWTP_sewer_rur <- wwtp_inputs$fcapacity*(sum((emissions$pathogen_rur_conforgrid*emissions$pathogen_rur_fconsewer),na.rm=TRUE))*(1-wwtp_inputs$removalfraction_sewer)
  wwtp_inputs$emissions_per_WWTP_onsite_urb <- wwtp_inputs$fcapacity*(sum((emissions$pathogen_urb_conforgrid*(1-emissions$pathogen_urb_fconsewer)),na.rm=TRUE))*(1-wwtp_inputs$removalfraction_onsite)
  wwtp_inputs$emissions_per_WWTP_onsite_rur <- wwtp_inputs$fcapacity*(sum((emissions$pathogen_rur_conforgrid*(1-emissions$pathogen_rur_fconsewer)),na.rm=TRUE))*(1-wwtp_inputs$removalfraction_onsite)
  wwtp_inputs$emissions_per_WWTP <- wwtp_inputs$emissions_per_WWTP_sewer_urb+wwtp_inputs$emissions_per_WWTP_sewer_rur+wwtp_inputs$emissions_per_WWTP_onsite_urb+wwtp_inputs$emissions_per_WWTP_onsite_rur
  
  log_warn("Warning: currently average treatment coverage and efficiency are used. No link is yet established with the pathogen flow tool")
  
  WWTP_emissions_per_subarea<-array(dim=c(length(unique(wwtp_inputs$subarea)),5))
  colnames(WWTP_emissions_per_subarea)<-c("subarea","sewer_urb","sewer_rur","onsite_urb","onsite_rur")
  WWTP_emissions_per_subarea<-as.data.frame(WWTP_emissions_per_subarea)
  
  for(j in 1:length(unique(wwtp_inputs$subarea))){
    temp<-which(wwtp_inputs$subarea==unique(wwtp_inputs$subarea)[j])
    WWTP_emissions_per_subarea$subarea[j]<-wwtp_inputs$subarea[temp[1]]
    WWTP_emissions_per_subarea$sewer_urb[j]<-sum(wwtp_inputs$emissions_per_WWTP_sewer_urb[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$sewer_rur[j]<-sum(wwtp_inputs$emissions_per_WWTP_sewer_rur[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$onsite_urb[j]<-sum(wwtp_inputs$emissions_per_WWTP_onsite_urb[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$onsite_rur[j]<-sum(wwtp_inputs$emissions_per_WWTP_onsite_rur[temp],na.rm=TRUE)
  }
  
  emissions$pathogen_urb_WWTP_sewer<-0
  emissions$pathogen_rur_WWTP_sewer<-0
  emissions$pathogen_urb_WWTP_onsite<-0
  emissions$pathogen_rur_WWTP_onsite<-0
  
  for(j in 1:length(WWTP_emissions_per_subarea$subarea)){
    emissions$pathogen_urb_WWTP_sewer[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_urb[j]
    emissions$pathogen_rur_WWTP_sewer[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_rur[j]
    emissions$pathogen_urb_WWTP_onsite[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_urb[j]
    emissions$pathogen_rur_WWTP_onsite[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_rur[j]
  }
  
  #now add the value to the correct grid in a new raster.
  
  pathogen_treatment[cellFromXY(ISORASTER,matrix(data=c(wwtp_inputs$Lon,wwtp_inputs$Lat),nrow=length(wwtp_inputs$Lon),ncol=2))]<-wwtp_inputs$emissions_per_WWTP
  return(list(emissions=emissions, treatment=pathogen_treatment))
}