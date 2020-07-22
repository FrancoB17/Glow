
readers.read.raster <- function(file_name_or_url){
  tic(sprintf("Read %s",file_name_or_url))
  r <- NULL
  if(startsWith(file_name_or_url,"http")){
    if(!dir.exists("./tmp")){
      dir.create("./tmp")
    }
    dest <- sprintf("./tmp/%s.tif",round(runif(1,1,10000)))
    log_info("Downloading raster from {file_name_or_url}")
    download.file(url = file_name_or_url,destfile = dest)
    r <- raster(dest)
  }
  else{
    r <- raster(file_name_or_url)
  }
  toc(log=T)
  return(r)
}

readers.read.wwtp <- function(file_name_or_url){
  wwtp_inputs <- NULL

  if(startsWith(file_name_or_url,"http")){
    file_path <- file_name_or_url
  }
  else{
    fname <- file_name_or_url
    if(!endsWith(fname,".csv")){
      fname <- sprintf("%s.csv",file_name_or_url)
    }
    file_path <- fname
  }
  wwtp_inputs <- read.csv(file_path, stringsAsFactors = F)
  
  return(wwtp_inputs)
}