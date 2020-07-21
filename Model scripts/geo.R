
geo.get.boundaries <- function(shpfile,source='GADM',...){
  if(!missing(shpfile)){
    boundaries <- readOGR(shpfile)
  }
  else if(source=="GADM"){
    boundaries <- gadm.get.data(...)
  }
  else{
    log_warn("Missing arguments")
  }
  return(boundaries)
}