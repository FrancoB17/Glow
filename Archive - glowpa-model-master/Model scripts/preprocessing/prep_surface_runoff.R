# Script to prepare the surface runoff part of total runoff of VIC data
# data received from Michelle van Vliet on 1-8-2017

rm(list=ls(all=TRUE))
library(sp)
library(rworldmap)
library(rgeos)
library(maptools)
library(raster)
library(rgdal)
library(RColorBrewer)
library(rworldxtra)

setwd("D:/LuciePhD/Data/VIC/raw")
data<- read.csv("Mon_FracSurfRunoff_19712000_WFD.csv", header=TRUE, col.names= c("cell_lon","cell_lat","cell_id","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#define coordinates
x<-data$cell_lon
y<-data$cell_lat
coordinates(data)<- data[c("cell_lat", "cell_lon")]

rm(x,y)
#tell R that the data are gridded
gridded(data)<-TRUE

plot(data)

#create different rasters for each of the columns with values in the data file
january<-raster(data, layer= c("Jan"))
plot(january)
february<-raster(data, layer= c("Feb"))
plot(february)
march<-raster(data, layer= c("Mar"))
plot(march)
april<-raster(data, layer= c("Apr"))
plot(april)
may<-raster(data, layer= c("May"))
plot(may)
june<-raster(data, layer= c("Jun"))
plot(june)
july<-raster(data, layer= c("Jul"))
plot(july)
august<-raster(data, layer= c("Aug"))
plot(august)
september<-raster(data, layer= c("Sep"))
plot(september)
october<-raster(data, layer= c("Oct"))
plot(october)
november<-raster(data, layer= c("Nov"))
plot(november)
december<-raster(data, layer= c("Dec"))
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

setwd("D:/LuciePhD/Data/VIC/processed/runoff")
writeRaster(jan,filename="f_surfacerunoff_jan.asc",format="ascii",overwrite=TRUE)
writeRaster(feb,filename="f_surfacerunoff_feb.asc",format="ascii",overwrite=TRUE)
writeRaster(mar,filename="f_surfacerunoff_mar.asc",format="ascii",overwrite=TRUE)
writeRaster(apr,filename="f_surfacerunoff_apr.asc",format="ascii",overwrite=TRUE)
writeRaster(may,filename="f_surfacerunoff_may.asc",format="ascii",overwrite=TRUE)
writeRaster(jun,filename="f_surfacerunoff_jun.asc",format="ascii",overwrite=TRUE)
writeRaster(jul,filename="f_surfacerunoff_jul.asc",format="ascii",overwrite=TRUE)
writeRaster(aug,filename="f_surfacerunoff_aug.asc",format="ascii",overwrite=TRUE)
writeRaster(sep,filename="f_surfacerunoff_sep.asc",format="ascii",overwrite=TRUE)
writeRaster(oct,filename="f_surfacerunoff_oct.asc",format="ascii",overwrite=TRUE)
writeRaster(nov,filename="f_surfacerunoff_nov.asc",format="ascii",overwrite=TRUE)
writeRaster(dec,filename="f_surfacerunoff_dec.asc",format="ascii",overwrite=TRUE)




######## make monthly grids of surface runoff ##############
#read total runoff grids
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/runoff")
raster_data <- list.files(path="D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/runoff") 
r<-stack(raster_data)

#read fraction surface runoff grids
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/f_surfacerunoff")
raster_data <- list.files(path="D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/f_surfacerunoff") 
s<-stack(raster_data)

#multiple total runoff with fraction of discharge coming from surface runoff
#this rasterbrick contains surface runoff rasters
r_surface<-r*s
#round to 3 digits to prevent excessively many numbers
r_surface_rounded<-round(r_surface, digits=3)
filenames<-paste0("surface", raster_data)
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/surfacerunoff")
writeRaster(r_surface_rounded, filename=filenames, format="ascii", bylayer=TRUE, overwrite=TRUE)

