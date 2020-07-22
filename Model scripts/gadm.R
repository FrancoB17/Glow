library(sf)

gadm.get.data <- function(gadm_gids,wkt_filter,country,level,gpkg_path){
  tic("Get GADM data")
  data <- NULL
  if(!missing(gpkg_path) && (!missing(gadm_gids) || !missing(wkt_filter) ) && !missing(level)){
    layer <- sprintf("level%s",level)
    if(!missing(gadm_gids)){
      gid_field <- sprintf("GID_%s",level)
      gids_str <- paste("'",gadm_gids,"'",collapse = ",",sep="")
      # construct select SQL query
      sql_q <- sprintf('SELECT * FROM %s WHERE %s IN (%s)',layer,gid_field,gids_str)
      sf_data <- st_read(gpkg_path,layer=layer,query=sql_q)
    }
    if(!missing(wkt_filter)){
      sf_data <- st_read(ENV$gadm_file,wkt_filter = wkt_filter)
      # remove null geometries, because they give errors in conversion to spatial object
      sf_data <- sf_data[!st_is_empty(sf_data),]
    }
    
    data <- as_Spatial(sf_data)
  }
  else if(!missing(country)){
    if(!dir.exists("./downloads/gadm")){
      dir.create("./downloads/gadm",recursive = T)
    }
    data <- getData(country=country,level=level,path="./downloads/gadm")
  }
  else{
    log_warn("Missing arguments")
  }
  toc(log=T)
  return(data)
}