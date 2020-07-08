mapping.tool.get.config <- function(){
  config <- data.frame(run=1)
  config$subareas <- "districts"
  config$pathogen <- "cryptosporidium"
  config$livestock_switch <- FALSE
  config$incidence_available <- FALSE
  config$wwtp_available <- 3
  config$hydrology_available <- FALSE
  config$interest_concentrations <- FALSE
  config$resolution <- 0.08333
  config$isoraster_filename <- "isoraster.asc"
  config$HumanData_filename <- "input_data.csv"
  config$WWTPData_filename <- "wwtp_input_data.csv"
  config$population_urban_filename <- "urban.pop.grd"
  config$population_rural_filename <- "rural.pop.grd"
  return(config)
}