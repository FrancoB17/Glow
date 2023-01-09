#Running the model + sensitivity analysis for crypto concentrations
# consists of running the following scripts:
# - diffuse_runoff.R
# - river_geometry.R
# - survival_instream.R
# - routing.R
# Calculation of the human and animal loads is NOT done in here
# assumed that this is done previously already

#Adapted by Nynke in April 2019 to run more smoothly (removed the path names throughout the scripts and subscripts)
#the model now only incorporates human diffuse and direct emissions and only one standard run is run.

#Adapted by Nynke in May 2019 to run from GloWPa_model.R


Sys.time()
source(paste0(working.path.script,"diffuse_runoff.R"))
print("finished runoff")
Sys.time()
source(paste0(working.path.script,"river_geometry.R"))
print("finished river geometry")
Sys.time()
source(paste0(working.path.script,"survival_instream.R"))
print("finished survival instream")
Sys.time()
source(paste0(working.path.script,"routing.R"))
print("finished routing")
Sys.time()
  



