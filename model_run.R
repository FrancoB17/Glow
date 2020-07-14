#' This script can be used to run the model in an R or Rstudio environment using the overall_inputs file to startup the model
#'

rm(list = ls())
# SET PATHS
model_dir <- "/home/nauta/Data/Software/GloWPa/"
model_input_dir <- "/home/nauta/Data/Projects/2020/K2P/02_Experiment/02_Runs/Run0/Input/kla_online/"
model_output_dir <- "/home/nauta/Data/Projects/2020/K2P/02_Experiment/02_Runs/Run0/Output/"
csv_sep <- ";"
# ----

setwd(model_dir)
source("./Model scripts/GloWPa.R")

scenarios <- read.csv(file.path(model_input_dir,"overall_inputs.csv"),sep = csv_sep, stringsAsFactors = F)

for(i in 1:dim(scenarios)[1]){
  scenario <- scenarios[i,]
  # set loadings_module to 1; human_emissions if not specified
  if(!"loadings_module" %in% colnames(scenario)){
    scenario$loadings_module <- 2
  }
  if(!"model_input" %in% colnames(scenario)){
    scenario$model_input <- model_input_dir
  }
  if(!"model_output" %in% colnames(scenario)){
    scenario$model_output <- model_output_dir
  }
  human_data_fname <- sprintf("%s.csv",scenario$HumanData_filename)
  human_data <- read.csv(file.path(model_input_dir,human_data_fname))
  human_data <- human_data[order(human_data$iso),]
  output <- glowpa.run(scenario,human_data)
}
