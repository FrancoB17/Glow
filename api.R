#' This file can be used to run gloWPa using REST api calls. Start api by api <- plumber::plumb(api.R) and then api$run()
library(plumber)
library(jsonlite)
source("Model scripts/GloWPa.R")

#' @post /scenario
function(paramsJSON,human_data){
  human_data <- read.csv(text = human_data)
  params <- fromJSON(paramsJSON)
  results <- glowpa.run(params,human_data)
  # TODO: emissions to json
  # TODO: geotiff location in response
  list(ouput = "output here")
}

#' @get /state
function(){
  STATE
}

#' @get /log
function(){
  
}
