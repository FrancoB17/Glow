#This code is the main code to run the GloWPa model. The model can be run for various waterborne pathogens
#and and various spatial scales and resolutions. This code is the basis for the K2P mapping tool, among others.
#This code was written by Nynke Hofstra in May 2019

#still to do: 
# add possible multiplying variables for all other variables than the ones already done
# test the makeisogrids part
# do we need to perform some checks everywhere to make sure the data are in the right format? We probably do. To be done.

#In case you want to set up scenarios, you can simply change the file overall_inputs.csv 
#by adding more lines.

# Set paths ---------------------------------------------------------------

rm(list=ls(all=TRUE))
model_path<<-"/home/nauta/Data/Software/GloWPa/"
wd<<-paste0(model_path,"Model output/Uganda/")
working.path.in<<-paste0(model_path,"Model input/Uganda/")
working.path.in.pathogens<<-paste0(model_path,"Model input/")
working.path.script<<-paste0(model_path,"Model scripts/")
working.path.out<<-paste0(model_path,"Model output/Uganda/")

# SET ISO READ FORMAT -------------------------------------------------------

# Options 
#ISO_READ_FORMATS <- list(POLYGON=1,RASTER=2)
EMISSION_MODULES <- list(NYNKE=1,MATT=2)
TOOL_MODES <- list(ON=T,OFF=F) 
# Set option here
#iso_read_format <- ISO_READ_FORMATS$POLYGON
emissions_module <- EMISSION_MODULES$MATT
tool_mode <- TOOL_MODES$ON
#############AFTER THIS LINE THERE IS NO NEED TO CHANGE ANYTHING IN THIS SCRIPT OR OTHER SCRIPTS#####################
setwd(wd)
source(file.path(working.path.script,"GloWPa configuration.R"))
# Open the overall input files and select the runs ------------------------

# For the online tool, this should not be read in from a .csv file, but be initialised in the code
# so there needs to be a switch selecting which of the two (model in R or tool with Rplumber)
# will have to be run.

# run original GloPWa
if(tool_mode){
  #read in overall input file
  config <- read.csv(paste0(working.path.in,"overall_inputs.csv"),stringsAsFactors=FALSE)
} else{
  # run GloWPa for mapping tool
  source(file.path(working.path.script,"mapping_tool_init.R"))
  config <- mapping.tool.get.config()
  #input_file <- read.csv(file.path(working.path.in,"input_data.csv"))
}


model.start(config,emissions_module,scenario_mode)


# End to open the overall input files and run selection -------------------