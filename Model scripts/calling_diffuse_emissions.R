## Running the model + sensitivity analysis for diffuse emissions
# this script calls manure_storage.R and diffuse_emissions.R


#Remarks on running:
# the first run is the standard run, all other runs are sensitivity runs with changed parameters
# if you want to run only the standard model and not the sensitivity analysis, just do run 1
# the file containing the parameters of the different runs is called cryptoL1_parameters.xlsx
# make changes in the .xlsx and save it as .csv for clean reading in 
# check this xlsx file to see what the different runs entail

#Remarks on the results:
# Many results are calculated multiple times for different subsets, see below
# --> split for the different continents
# --> split for to land directly, to storage with dieoff, to storage without dieoff
# --> split for the different animal species
# unless you want to analyse for these subsets, it is not necessary to look at these specific results

rm(list=ls(all=TRUE))
 setwd("D:/LuciePhD/Model/Cryptomodel/temporary")
  library(sp)
  library(rgeos)
  library(maptools)
  library(rworldmap)
  library(raster)
  library(RColorBrewer)
  library(rworldxtra)
  library(rgdal)
  working.path.in<-"D:/LuciePhD/Model/Cryptomodel/modeldata/"
  working.path.out<-"D:/LuciePhD/Model/Cryptomodel/temporary/"
  
  LandArea <- (readAsciiGrid(paste(working.path.in,"GAREALAND.map",sep=""))) #in km2 per grid
  landraster<- raster(LandArea)
  animals<-read.csv("D:/LuciePhD/Data/FAO animal data/Extract2010.csv")
  countries<-raster(paste(working.path.in,"countries30min2010.asc",sep=""))
  
  #define color palette to be used in map plots. Usage: plot(..., col=colors)
  colors <-colorRampPalette(c("green", "yellow", "orange","red"))(100)
  colors2<-colorRampPalette(c("blue","blue", "blue", "dodgerblue", "dodgerblue", "dodgerblue", "green", "green", "yellow", "orange","red", "red4"))(100)
  colors4<-colorRampPalette(c("blue","dodgerblue", "green", "yellow", "orange","red", "red4"))(100)
  
  
    ## Read in overall parameters file
  par<- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/cryptoL1_parameters.csv" )

  runs<-79 #number

###   define results file and add columns #######
  run_number<-c(1:runs)
  results<-data.frame(run_number)
  results$cattletotal<-NA
  results$cattleyoung<-NA
  results$cattleadult<-NA
  results$buffalototal<-NA
  results$buffaloyoung<-NA
  results$buffaloadult<-NA
  results$goattotal<-NA
  results$goatyoung<-NA
  results$goatadult<-NA
  results$sheeptotal<-NA
  results$sheepyoung<-NA
  results$sheepadult<-NA
  results$pigtotal<-NA
  results$pigyoung<-NA
  results$pigadult<-NA
  results$chickentotal<-NA
  results$ducktotal<-NA
  results$horsetotal<-NA
  results$asstotal<-NA
  results$cameltotal<-NA
  results$muletotal<-NA
  results$cattletotal_log<-NA
  results$cattleyoung_log<-NA
  results$cattleadult_log<-NA
  results$buffalototal_log<-NA
  results$buffaloyoung_log<-NA
  results$buffaloadult_log<-NA
  results$goattotal_log<-NA
  results$goatyoung_log<-NA
  results$goatadult_log<-NA
  results$sheeptotal_log<-NA
  results$sheepyoung_log<-NA
  results$sheepadult_log<-NA
  results$pigtotal_log<-NA
  results$pigyoung_log<-NA
  results$pigadult_log<-NA
  results$chickentotal_log<-NA
  results$ducktotal_log<-NA
  results$horsetotal_log<-NA
  results$asstotal_log<-NA
  results$cameltotal_log<-NA
  results$muletotal_log<-NA
  
### results split for continents ##########  
  results$africatotal<-NA
  results$asiatotal<-NA
  results$europetotal<-NA
  results$latinamericatotal<-NA
  results$menatotal<-NA
  results$northamericatotal<-NA
  results$oceaniatotal<-NA
  
  results$africatotal_L<-NA
  results$asiatotal_L<-NA
  results$europetotal_L<-NA
  results$latinamericatotal_L<-NA
  results$menatotal_L<-NA
  results$northamericatotal_L<-NA
  results$oceaniatotal_L<-NA
  
  results$africatotal_S<-NA
  results$asiatotal_S<-NA
  results$europetotal_S<-NA
  results$latinamericatotal_S<-NA
  results$menatotal_S<-NA
  results$northamericatotal_S<-NA
  results$oceaniatotal_S<-NA
  
  results$africatotal_S_survived<-NA
  results$asiatotal_S_survived<-NA
  results$europetotal_S_survived<-NA
  results$latinamericatotal_S_survived<-NA
  results$menatotal_S_survived<-NA
  results$northamericatotal_S_survived<-NA
  results$oceaniatotal_S_survived<-NA
  
### results split for to land directly, to storage with dieoff, to storage without dieoff ########
  
  results$cattletotal_L<-NA
  results$cattleyoung_L<-NA
  results$cattleadult_L<-NA
  results$buffalototal_L<-NA
  results$buffaloyoung_L<-NA
  results$buffaloadult_L<-NA
  results$goattotal_L<-NA
  results$goatyoung_L<-NA
  results$goatadult_L<-NA
  results$sheeptotal_L<-NA
  results$sheepyoung_L<-NA
  results$sheepadult_L<-NA
  results$pigtotal_L<-NA
  results$pigyoung_L<-NA
  results$pigadult_L<-NA
  results$chickentotal_L<-NA
  results$ducktotal_L<-NA
  results$horsetotal_L<-NA
  results$asstotal_L<-NA
  results$cameltotal_L<-NA
  results$muletotal_L<-NA
  
  results$cattletotal_S<-NA
  results$cattleyoung_S<-NA
  results$cattleadult_S<-NA
  results$buffalototal_S<-NA
  results$buffaloyoung_S<-NA
  results$buffaloadult_S<-NA
  results$goattotal_S<-NA
  results$goatyoung_S<-NA
  results$goatadult_S<-NA
  results$sheeptotal_S<-NA
  results$sheepyoung_S<-NA
  results$sheepadult_S<-NA
  results$pigtotal_S<-NA
  results$pigyoung_S<-NA
  results$pigadult_S<-NA
  results$chickentotal_S<-NA
  results$ducktotal_S<-NA
  results$horsetotal_S<-NA
  results$asstotal_S<-NA
  results$cameltotal_S<-NA
  results$muletotal_S<-NA
  
  results$cattletotal_S_survived<-NA
  results$cattleyoung_S_survived<-NA
  results$cattleadult_S_survived<-NA
  results$buffalototal_S_survived<-NA
  results$buffaloyoung_S_survived<-NA
  results$buffaloadult_S_survived<-NA
  results$goattotal_S_survived<-NA
  results$goatyoung_S_survived<-NA
  results$goatadult_S_survived<-NA
  results$sheeptotal_S_survived<-NA
  results$sheepyoung_S_survived<-NA
  results$sheepadult_S_survived<-NA
  results$pigtotal_S_survived<-NA
  results$pigyoung_S_survived<-NA
  results$pigadult_S_survived<-NA
  results$chickentotal_S_survived<-NA
  results$ducktotal_S_survived<-NA
  results$horsetotal_S_survived<-NA
  results$asstotal_S_survived<-NA
  results$cameltotal_S_survived<-NA
  results$muletotal_S_survived<-NA
  
### results split for intensive extensive #######
  results$cattletotal_I<-NA
  results$buffalototal_I<-NA
  results$goattotal_I<-NA
  results$sheeptotal_I<-NA
  results$pigtotal_I<-NA
  results$chickentotal_I<-NA
  results$ducktotal_I<-NA
  results$horsetotal_I<-NA
  results$asstotal_I<-NA
  results$cameltotal_I<-NA
  results$muletotal_I<-NA

####### the loop ##########  
  
  Sys.time()
  
for (i in 1:runs){
  
  source("D:/LuciePhD/Model/RScripts/manure_storage.R")
  source("D:/LuciePhD/Model/RScripts/diffuse_emissions.R")
  print(i)
  
}

  filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/results",Sys.Date(), ".csv")
  write.csv(results,file=filename) 

  Sys.time()
  
