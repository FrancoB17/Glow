
emissions.calc.avg.pp <- function(emissions){
  emissions$pathogen_urb_waterforgrid_pp <- emissions$pathogen_urb_waterforgrid/HUMAN_DATA$popurb
  emissions$pathogen_rur_waterforgrid_pp <- emissions$pathogen_rur_waterforgrid/HUMAN_DATA$poprur
  
  emissions$pathogen_urb_landforgrid_pp <- emissions$pathogen_urb_landforgrid/HUMAN_DATA$popurb
  emissions$pathogen_rur_landforgrid_pp <- emissions$pathogen_rur_landforgrid/HUMAN_DATA$poprur
  return(emissions)
}

emissions.na.replace <- function(emissions,fill=0){
  browser()
  cols <- c("pathogen_urb_conforgrid","pathogen_rur_conforgrid","pathogen_urb_waterforgrid","pathogen_rur_waterforgrid","pathogen_urb_landforgrid","pathogen_rur_landforgrid",
            "pathogen_urb_con","pathogen_rur_con","pathogen_urb_onsite_land","pathogen_rur_onsite_land","pathogen_urb_dif","pathogen_rur_dif")
  for(col_name in cols){
    na_idx <- which(is.na(emissions[[col_name]]))
    if(length(na_idx)>0){
      emissions[[na_idx,col_name]] <- fill
    }
  }
  return(emissions)
}

emissions.calc.fconsewer <- function(emissions){
  browser()
  emissions$pathogen_urb_fconsewer<-0
  emissions$pathogen_rur_fconsewer<-0
  
  a<-which(emissions$pathogen_urb_conforgrid>0)
  b<-which(emissions$pathogen_rur_conforgrid>0)
  
  if(length(a)>0){
    emissions$pathogen_urb_fconsewer[a]<-emissions$pathogen_urb_con[a]/emissions$pathogen_urb_conforgrid[a]
  }
  if(length(b)>0){
    emissions$pathogen_rur_fconsewer[b]<-emissions$pathogen_rur_con[b]/emissions$pathogen_rur_conforgrid[b]
  }
  return(emissions)
}