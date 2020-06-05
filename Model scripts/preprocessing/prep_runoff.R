#SCript of Marijke van Hengel
#based on preprocessing_discharge.R made by Lucie Vermeulen

# 2-2-2015
# This script processes the VIC runoff data into ASCII grid files.

rm(list=ls(all=TRUE))
library(sp)
library(rworldmap)
library(rgeos)
library(maptools)
library(raster)
library(RColorBrewer)
library(rworldxtra)

setwd("E:/modelinput/climate/WaterCycle/VIC/raw")
data<- read.csv("monthly_runoff.csv", header=TRUE, col.names= c("cell_lon","cell_lat","cell_id","Jan_Runoff","Feb_Runoff","Mar_Runoff","Apr_Runoff","May_Runoff","Jun_Runoff","Jul_Runoff","Aug_Runoff","Sep_Runoff","Oct_Runoff","Nov_Runoff","Dec_Runoff"))

library("sp")
library("rgdal")
library("raster")

#define coordinates
x<-data$cell_lon
y<-data$cell_lat
coordinates(data)<- data[c("cell_lat", "cell_lon")]

rm(x,y)
#tell R that the data are gridded
gridded(data)<-TRUE

plot(data)

#create different rasters for each of the columns with values in the data file
january<-raster(data, layer= c("Jan_Runoff"))
plot(january)
february<-raster(data, layer= c("Feb_Runoff"))
plot(february)
march<-raster(data, layer= c("Mar_Runoff"))
plot(march)
april<-raster(data, layer= c("Apr_Runoff"))
plot(april)
may<-raster(data, layer= c("May_Runoff"))
plot(may)
june<-raster(data, layer= c("Jun_Runoff"))
plot(june)
july<-raster(data, layer= c("Jul_Runoff"))
plot(july)
august<-raster(data, layer= c("Aug_Runoff"))
plot(august)
september<-raster(data, layer= c("Sep_Runoff"))
plot(september)
october<-raster(data, layer= c("Oct_Runoff"))
plot(october)
november<-raster(data, layer= c("Nov_Runoff"))
plot(november)
december<-raster(data, layer= c("Dec_Runoff"))
plot(december)


#the extent of the current rasters does not cover the whole world in the y direction (latitude)
#so we need to define the desired extent
e<-extent(-180,180,-90,90)

#and extend the rasters to the desired extent (entire globe)
jan<-extend(january, e, value=NA)
feb<-extend(february, e, value=NA)
mar<-extend(march, e, value=NA)
apr<-extend(april, e, value=NA)
may<-extend(may, e, value=NA)
jun<-extend(june, e, value=NA)
jul<-extend(july, e, value=NA)
aug<-extend(august, e, value=NA)
sep<-extend(september, e, value=NA)
oct<-extend(october, e, value=NA)
nov<-extend(november, e, value=NA)
dec<-extend(december, e, value=NA)

setwd("E:/modelinput/climate/WaterCycle/VIC/processed/runoff")
writeRaster(jan,filename="runoff_jan.asc",format="ascii",overwrite=TRUE)
writeRaster(feb,filename="runoff_feb.asc",format="ascii",overwrite=TRUE)
writeRaster(mar,filename="runoff_mar.asc",format="ascii",overwrite=TRUE)
writeRaster(apr,filename="runoff_apr.asc",format="ascii",overwrite=TRUE)
writeRaster(may,filename="runoff_may.asc",format="ascii",overwrite=TRUE)
writeRaster(jun,filename="runoff_jun.asc",format="ascii",overwrite=TRUE)
writeRaster(jul,filename="runoff_jul.asc",format="ascii",overwrite=TRUE)
writeRaster(aug,filename="runoff_aug.asc",format="ascii",overwrite=TRUE)
writeRaster(sep,filename="runoff_sep.asc",format="ascii",overwrite=TRUE)
writeRaster(oct,filename="runoff_oct.asc",format="ascii",overwrite=TRUE)
writeRaster(nov,filename="runoff_nov.asc",format="ascii",overwrite=TRUE)
writeRaster(dec,filename="runoff_dec.asc",format="ascii",overwrite=TRUE)