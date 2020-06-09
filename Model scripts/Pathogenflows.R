#devtools::install_github('mverbyla/pathogenflows')
library(pathogenflows)

pathogenflow.model.run <- function(input_data,pathogen){
  emissions <- data.frame()
  # 1) extract pathogen type from pathogen
  pathogenType <- "Virus"
  # 2a) prepare data for urban and rural
  
  # 2b) call loadings function twice for urban and rural areas
  onsite_urban = data.frame()
  onsite_rural = data.frame()
  rural_loadings <- pathogenflows::getLoadings(onsite_rural,pathogenType)
  urban_loadings <- pathogenflows::getLoadings(onsite_urban,pathogenType)
  # 2c) postprocess output
  
  # 3) create similair emissions data.frame as original human emissions
  
  # 4) return emissions
  return(emissions)
}