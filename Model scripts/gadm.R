library(sf)

gadm.get.data <- function(gadm_gids,country,level,gpkg_path){
  data <- NULL
  if(!missing(gpkg_path) && !missing(gadm_gids) && !missing(level)){
    layer <- sprintf("level%s",level)
    gid_field <- sprintf("GID_%s",level)
    gids_str <- paste("'",gadm_gids,"'",collapse = ",",sep="")
    # construct select SQL query
    sql_q <- sprintf('SELECT * FROM %s WHERE %s IN (%s)',layer,gid_field,gids_str)
    sf_data <- st_read(gpkg_path,layer=layer,query=sql_q)
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
  return(data)
}