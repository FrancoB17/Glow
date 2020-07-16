
readers.read.raster <- function(file_name_or_url){
  r <- NULL
  if(startsWith(file_name_or_url,"http")){
    # TODO: download to temp
    dest <- file.path("./downloads",Sys.getpid(), basename(file_name_or_url))
    dirname <- dirname(dest)
    dir.create(dirname,recursive = T,showWarnings = F)
    download.file(url = file_name_or_url,destfile = dest)
    r <- raster(dest)
    #unlink(file.path("./downloads",Sys.getpid()),recursive = T)
  }
  else{
    r <- raster(file_name_or_url)
  }
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