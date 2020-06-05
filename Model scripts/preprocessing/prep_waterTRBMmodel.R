# this script is made to preprocess the WAter temperature data
# as obtained from Michellen on 04-05-2017
# these are Water T data output from the RBM model
# I'm going to compare these with water T data calculated from
# WATCH forcing air T using the regression model by Punzet et al. 2012

#read in data
RBM<-read.table("D:/LuciePhD/Data/Water T/MeanMon_Tw_WFD_19712000.txt", header=TRUE)

library(raster)
mons <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
mons_alpha<-c("apr","aug","dec","feb","jan","jul","jun","mar","may","nov","oct","sep")
Mons <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tw_RBM")

for(i in 1:12){
#make new dataframe with columns lat, long, and monthly values
df<-data.frame(RBM$cell_lat,RBM$cell_lon,RBM[,i+3])
# create spatial points data frame
spg <- df
coordinates(spg) <- ~ RBM.cell_lon + RBM.cell_lat
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
rasterDF <- raster(spg)
plot(rasterDF, main=paste0("Tw ",mons[i]))
e <- extent(-180, 180, -90, 90)
rasterDF_extended <- extend(rasterDF, e)
plot(rasterDF_extended, main=paste0("Tw ",mons[i]))
writeRaster(rasterDF_extended,filename = paste0("Tw_RBM_",mons[i],".asc"),format="ascii",overwrite=TRUE)
}

#read in the various Water T rasters and compare
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tw")
Tw_files<- list.files(path=("D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/archive/Tw")) 
Tw<-stack(Tw_files)
plot(Tw,col=colors4)

setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tw_RBM")
Tw_RBM_files<- list.files(path=("D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tw_RBM")) 
Tw_RBM<-stack(Tw_RBM_files)
plot(Tw_RBM,col=colors4)

dif<-Tw_RBM-Tw
names(dif)<-mons_alpha
plot(dif,col=colors3)#, main="difference"
plot(dif$jan,col=colors4,main="difference jan")
plot(Tw_RBM$Tw_RBM_jan,col=colors4,main="Tw jan RBM")
plot(Tw$Tw_jan,col=colors4,main="Tw jan regression Punzet")
plot(dif$apr,col=colors4,main="difference apr")
plot(Tw_RBM$Tw_RBM_apr,col=colors4,main="Tw apr RBM")
plot(Tw$Tw_apr,col=colors4,main="Tw apr regression Punzet")
plot(dif$jul,col=colors4,main="difference jul")
plot(Tw_RBM$Tw_RBM_jul,col=colors4,main="Tw jul RBM")
plot(Tw$Tw_jul,col=colors4,main="Tw jul regression Punzet")
plot(dif$oct,col=colors4,main="difference oct")
plot(Tw_RBM$Tw_RBM_oct,col=colors4,main="Tw oct RBM")
plot(Tw$Tw_oct,col=colors4,main="Tw oct regression Punzet")

