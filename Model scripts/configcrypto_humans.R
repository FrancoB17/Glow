# Human emissions model configuration script - run this before running the script Crypto_final_humans.R
# Adjusted by Lucie so that human diffuse emissions will go to the animal diffuse emissions

#Remark: when running scenarios:
#1. HDI remains the same. That is not necessarily true. In 2050 more countries likely
#   have a much higher HDI. That will reduce the emissions.
#2. The population grid is equally multiplied with the population increase. This does
#   not increase urban population in particular. However, the programme does take
#   urbanisation separately, so more grids will be identified as urban. This causes
#   a possibly too large spread of the population over the grid, but on the other hand
#   really accounting for urbanisation may produce urban grids that contain more people
#   than could actually live in those grids. 

#Configuration Cryptosporidium


#####################Configuration#######################################################
rm(list=ls(all=TRUE))
configuration <- "standard"

#Set working directory, load libraries, set working path in and out

setwd("D:/LuciePhD/Model/Cryptomodel/temporary")
library(sp)
library(rworldmap)
library(rgeos)
library(maptools)
library(raster)
library(RColorBrewer)
library(rworldxtra)
working.path.in<-"D:/LuciePhD/Model/HumansData&ModelNynke/Model input/"
working.path.out<-"D:/LuciePhD/Model/Cryptomodel/modeloutput_humans/"


#Variables

scenario<- ""# "_ssp1","_ssp1star","ssp3","_ssp3"
HDIboundary <- 0.785

cryptoExcretionLowHDI_child<-1e8     #incidence_child_lowhdi*excretion*episodelength  #episodes per person per year * excretion per day * days per episode
cryptoExcretionHighHDI_child<-5e7    #incidence_child_highhdi*excretion*episodelength
cryptoExcretionLowHDI_others<-1e8    #incidence_others*excretion*episodelength
cryptoExcretionHighHDI_others<-5e7   #incidence_others*excretion*episodelength

cryptoRemovalTertiary <-  0.9775  #fraction removed 
cryptoRemovalSecondary <-	0.55	  #fraction removed
cryptoRemovalPrimary <- 0.1			  #fraction removed 

RuralDirect2Water <- 1  
UrbanDirect2Water <- 1
RuralNonSource2Water <- 0
UrbanNonSource2Water <- 0
RuralDiffuse2Water <- 1  #Runoff fraction is 1 here, will be accounted for later with animal sources.
UrbanDiffuse2Water <- 0

NappyCor<-0.5

#Datasets
Countries30minNoStates <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep=""))) 	#has ISO codes going to 904, leading to holes in some parts of Europe, needs fixing. HDI files made based on this file also need fixing
Population30min <- (readAsciiGrid(paste(working.path.in,"landscan2010.asc",sep="")))
#Fix some numbers that appear on this map erroneously
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 901)  #Normandy
Countries30minNoStates$Countries30minNoStates.asc[a]<-250           #France
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 902)  #Po Plain
Countries30minNoStates$Countries30minNoStates.asc[a]<-380           #Italy
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 903)  #north Spain
Countries30minNoStates$Countries30minNoStates.asc[a]<-724           #Spain
LandArea <- (readAsciiGrid(paste(working.path.in,"GAREALAND.map",sep="")))
population2010<-read.csv(paste(working.path.in,"Landscan2010sum.csv",sep=""),sep=",",header=TRUE,col.names=c("countryname","iso","pop_totalx1000"))
population2010<-population2010[order(population2010$iso),]
CountryDataPoint<-read.csv(paste(working.path.in,"Point_input_data_all",scenario,".csv",sep=""),sep=",",header=TRUE,col.names=c("countryname","iso","hdi","pop_totalx1000","f_urb","f_rur","f_under5","f_5to14","f_15to24","f_25plus","con_urb", "con_rur","dir_urb","dir_rur","dif_urb","dif_rur","non_urb","non_rur", "treatm_1","treatm_2","treatm_3","treatm_no"),numerals="warn.loss")
CountryDataPoint<-CountryDataPoint[order(CountryDataPoint$iso),]

#define color palette to be used in map plots
colors<-colorRampPalette(c("green", "yellow", "orange","red"))(100)