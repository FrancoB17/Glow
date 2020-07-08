wwtp.module.run <- function(emissions,pathogen,popurban_grid,poprural_grid){
  #open .csv file with WWTP data
  # if 3 then location of WWTP known
  # if 2 then treatment, but location unknown
  # if 1 then no treatment
  
  # if(wwtp_available==3){
  #   WWTP_inputs<-read.csv(paste0(working.path.in,overall_inputs$WWTPData_filename[i],".csv"))  #file has district, lat, lon, capacity, system categories. Would be good to add treatment type (if available) and reduction (if available) -> these data should come from Matt's model later on. For now just average removal has been used.
  #   WWTP_inputs$treatment_type<-as.character(WWTP_inputs$treatment_type)
  #   WWTP_inputs$subregion<-as.character(WWTP_inputs$subregion)
  # }
  
  # spatial distribution
  # from population to pathogens
  # for connected emissions in the case the location of WWTPs is known
  
  pathogen_treatment<-isoraster
  pathogen_treatment[]<-0
  
  # calculate the pathogen treathment of WWTP per subarea
  if(SCENARIO$wwtp_available ==3){
    
    WWTP_inputs <- read.csv(SCENARIO$WWTPData_filename)
    
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

    WWTP_inputs$subarea<-raster::extract(isoraster,matrix(data=c(WWTP_inputs$Lon,WWTP_inputs$Lat),nrow=length(WWTP_inputs$Lon),ncol=2))
    
    for(j in 1:length(WWTP_inputs$subarea)){

      if(identical(WWTP_inputs$treatment_type[j],"primary")){
        WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalPrimary[pathogen_row]
        WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalPrimary[pathogen_row]
      }else if(identical(WWTP_inputs$treatment_type[j],"secondary")){
        WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalSecondary[pathogen_row]
        WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalSecondary[pathogen_row]
      }else if(identical(WWTP_inputs$treatment_type[j],"tertiary")){
        WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalTertiary[pathogen_row]
        WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalTertiary[pathogen_row]
      }else if(identical(WWTP_inputs$treatment_type[j],"quaternary")){
        WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalQuaternary[pathogen_row]
        WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalQuaternary[pathogen_row]
      }else if(identical(WWTP_inputs$treatment_type[j],"ponds")){
        WWTP_inputs$removalfraction_sewer[j]<-pathogen$RemovalPonds[pathogen_row]
        WWTP_inputs$removalfraction_onsite[j]<-pathogen$RemovalPonds[pathogen_row]
      }else if(identical(WWTP_inputs$treatment_type[j],"not treated")){
        WWTP_inputs$removalfraction_sewer[j]<-0
        WWTP_inputs$removalfraction_onsite[j]<-0
      }
    }
    #estimate the fraction of people connected per capacity unit
    capacity<-sum(WWTP_inputs$Capacity,na.rm=TRUE)  #how to deal with capacity is NA -> WWTP non existing?
    WWTP_inputs$fcapacity<-WWTP_inputs$Capacity/capacity
    
    #calculate the emissions per WWTP
    #the removal file for the WWTP is used if specified from the Sketcher tool, or the removal rate from the
    #literature is used (specified in for loop above)
    WWTP_inputs$emissions_per_WWTP_sewer_urb<-WWTP_inputs$fcapacity*(sum((emissions$pathogen_urb_conforgrid*emissions$pathogen_urb_fconsewer),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_sewer)
    WWTP_inputs$emissions_per_WWTP_sewer_rur<-WWTP_inputs$fcapacity*(sum((emissions$pathogen_rur_conforgrid*emissions$pathogen_rur_fconsewer),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_sewer)
    WWTP_inputs$emissions_per_WWTP_onsite_urb<-WWTP_inputs$fcapacity*(sum((emissions$pathogen_urb_conforgrid*(1-emissions$pathogen_urb_fconsewer)),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_onsite)
    WWTP_inputs$emissions_per_WWTP_onsite_rur<-WWTP_inputs$fcapacity*(sum((emissions$pathogen_rur_conforgrid*(1-emissions$pathogen_rur_fconsewer)),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_onsite)
    WWTP_inputs$emissions_per_WWTP<-WWTP_inputs$emissions_per_WWTP_sewer_urb+WWTP_inputs$emissions_per_WWTP_sewer_rur+WWTP_inputs$emissions_per_WWTP_onsite_urb+WWTP_inputs$emissions_per_WWTP_onsite_rur
    
    print("Warning: currently average treatment coverage and efficiency are used. No link is yet established with the pathogen flow tool")
    
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
      emissions$pathogen_urb_WWTP_sewer[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_urb[j]
      emissions$pathogen_rur_WWTP_sewer[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_rur[j]
      emissions$pathogen_urb_WWTP_onsite[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_urb[j]
      emissions$pathogen_rur_WWTP_onsite[which(HUMAN_DATA$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_rur[j]
    }
    
    #now add the value to the correct grid in a new raster.

    pathogen_treatment[cellFromXY(isoraster,matrix(data=c(WWTP_inputs$Lon,WWTP_inputs$Lat),nrow=length(WWTP_inputs$Lon),ncol=2))]<-WWTP_inputs$emissions_per_WWTP
    
    emissions$pathogen_urb_con<-emissions$pathogen_urb_WWTP_sewer
    emissions$pathogen_rur_con<-emissions$pathogen_rur_WWTP_sewer
    emissions$pathogen_urb_onsite_treated<-emissions$pathogen_urb_WWTP_onsite
    emissions$pathogen_rur_onsite_treated<-emissions$pathogen_rur_WWTP_onsite
    
  }
  
  pathogen_urban_water_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_urb_waterforgrid_pp)
  pathogen_urban_water_pp_raster<-subs(isoraster, pathogen_urban_water_pp , subsWithNA=T)
  
  pathogen_rural_water_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_rur_waterforgrid_pp)
  pathogen_rural_water_pp_raster<-subs(isoraster, pathogen_rural_water_pp , subsWithNA=T)
  
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
  pathogen_urban_land_pp_raster<-subs(isoraster, pathogen_urban_land_pp , subsWithNA=T)
  
  pathogen_rural_land_pp<-data.frame(iso=emissions$iso,value=emissions$pathogen_rur_landforgrid_pp)
  pathogen_rural_land_pp_raster<-subs(isoraster, pathogen_rural_land_pp , subsWithNA=T)
  
  pathogen_urban_land_grid<-pathogen_urban_land_pp_raster*popurban_grid
  pathogen_rural_land_grid<-pathogen_rural_land_pp_raster*poprural_grid
  
  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)
  
  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)
  
  pathogen_land_grid<-pathogen_urban_land_grid+pathogen_rural_land_grid
    
  return(list(emissions=emissions,grids=list(land=pathogen_land_grid,water=pathogen_water_grid)))
  
}