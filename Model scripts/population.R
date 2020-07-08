#' This script contains functions regarding the population data processing

population.preprocess <- function(){
  #read
  popurban_grid<-raster(file.path(SCENARIO$model_input,SCENARIO$population_urban_filename))
  poprural_grid<-raster(file.path(SCENARIO$model_input,SCENARIO$population_rural_filename))
  populations <- list(urban=poprural_grid,rural=poprural_grid)
  # validate
  population.validate(populations)
  extent <- extent(ISORASTER)
  # crop to extent
  populations <- lapply(populations,FUN = function(grid){
    return(crop(grid,extent))
  })
  # correct with HumanData values
  populations <- population.correct(populations)
  return(populations)
}

population.validate <-function(populations){
  # validate all population grids
  reso_valid <- lapply(population_grids,FUN = validate.resolution)
  # must be all valid
  if(!all(reso_valid)){
    print("ERROR: population file(s) are incorrect resolution!")
    quit
  }
  
}

population.correct <- function(populations){
  library(dplyr)
  all_data <- data.frame(Urban = values(populations$urban),Rural = values(populations$rural),ISO = values(ISORASTER))
  summed <- all_data %>%
    filter(ISO %in% HUMAN_DATA$iso) %>% # restrict to only ISOs in HumanData_iso
    group_by(ISO) %>% # do the following per ISO code
    summarise(UrbanTotal = sum(Urban, na.rm = TRUE),
              RuralTotal = sum(Rural, na.rm = TRUE)) %>%
    ungroup()
  HUMAN_DATA <- HUMAN_DATA[(HUMAN_DATA$iso %in% summed$ISO), ]
  
  HUMAN_DATA$popurb<-NA
  HUMAN_DATA$poprur<-NA
  
  for (j in 1:length(HUMAN_DATA$iso)){
    q<-which(summed$ISO==HUMAN_DATA$iso[j])
    if(length(q)>0){
      HUMAN_DATA$popurb[q]<-summed$UrbanTotal[q]
      HUMAN_DATA$poprur[q]<-summed$RuralTotal[q]
    }
  }
  detach("package:dplyr", unload=TRUE)
  
  urban_pop<-HUMAN_DATA$f_urb*HUMAN_DATA$pop_total
  rural_pop<-(1-HUMAN_DATA$f_urb)*HUMAN_DATA$pop_total
  
  dif_urban<-urban_pop/HUMAN_DATA$popurb
  dif_rural<-rural_pop/HUMAN_DATA$poprur
  
  dif_urban<-data.frame(iso=HUMAN_DATA$iso,value=dif_urban)
  dif_urban_raster<-subs(isoraster, dif_urban, subsWithNA=T)
  
  dif_rural<-data.frame(iso=HUMAN_DATA$iso,value=dif_rural)
  dif_rural_raster<-subs(isoraster, dif_rural, subsWithNA=T)
  
  populations$urban<-populations$urban*dif_urban_raster
  populations$rural<-populations$rural*dif_rural_raster
  
  return(populations)
}


validate.resolution <- function(grid){
  if( reso-xres(grid) > 1e-5 || reso-yres(grid)>1e-5){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}
