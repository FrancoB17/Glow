#preprocessing discharge



###############################################################################################
#############################  Mean monthly hydrological data conversion xls to asc grid ######

#first removed 1 line header and converted xls file to csv, to simplify reading in the data
#these were created with the VIC model with WATCH forcing data
#they are monthly average discharge values in m3/s

setwd("D:/LuciePhD/Data/VIC/RCPs/preprocessed/")

model<-"noresm1-m" #gfdl-esm2m , hadgem2-es , ipsl-cm5a-lr , miroc-esm-chem , noresm1-m
rcp_num<-"_HistAndRcp8p5" # _HistAndRcp2p6 ; _HistAndRcp8p5
period<-"fut2050s_20402069_" #fut2050s_20402069_  ctrl_19712000_
rcp<-paste0(period,model,rcp_num)
data<- read.table(paste0("E:/Data/VIC/RCPs/monthly_TotRunoff_Flow_",rcp,".txt"), header=FALSE, col.names= c("cell_lat","cell_lon","cell_id","Jan_meanQ_WFD_1980s","Feb_meanQ_WFD_1980s","Mar_meanQ_WFD_1980s","Apr_meanQ_WFD_1980s","May_meanQ_WFD_1980s","Jun_meanQ_WFD_1980s","Jul_meanQ_WFD_1980s","Aug_meanQ_WFD_1980s","Sep_meanQ_WFD_1980s","Oct_meanQ_WFD_1980s","Nov_meanQ_WFD_1980s","Dec_meanQ_WFD_1980s","Jan_Runoff","Feb_Runoff","Mar_Runoff","Apr_Runoff","May_Runoff","Jun_Runoff","Jul_Runoff","Aug_Runoff","Sep_Runoff","Oct_Runoff","Nov_Runoff","Dec_Runoff"))

library("sp")
#install.packages("rgdal")
library("rgdal")
library("raster")

#define coordinates
x<-data$cell_lon
y<-data$cell_lat
coordinates(data)<- data[c("cell_lon", "cell_lat")]

#tell R that the data are gridded
gridded(data)<-TRUE
plot(data)

#create different rasters for each of the columns with values in the data file
january<-raster(data, layer= c("Jan_meanQ_WFD_1980s"))
plot(january)
february<-raster(data, layer= c("Feb_meanQ_WFD_1980s"))
plot(february)
march<-raster(data, layer= c("Mar_meanQ_WFD_1980s"))
plot(march)
april<-raster(data, layer= c("Apr_meanQ_WFD_1980s"))
plot(april)
may<-raster(data, layer= c("May_meanQ_WFD_1980s"))
plot(may)
june<-raster(data, layer= c("Jun_meanQ_WFD_1980s"))
plot(june)
july<-raster(data, layer= c("Jul_meanQ_WFD_1980s"))
plot(july)
august<-raster(data, layer= c("Aug_meanQ_WFD_1980s"))
plot(august)
september<-raster(data, layer= c("Sep_meanQ_WFD_1980s"))
plot(september)
october<-raster(data, layer= c("Oct_meanQ_WFD_1980s"))
plot(october)
november<-raster(data, layer= c("Nov_meanQ_WFD_1980s"))
plot(november)
december<-raster(data, layer= c("Dec_meanQ_WFD_1980s"))
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


#create annual average discharge raster
annual<-(jan+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec)/12
plot(annual)

##write rasters to ascii grids
#writeRaster(jan,"dischargeVIC_average19712000_january.asc",format="ascii", overwrite=TRUE)
#writeRaster(feb,"dischargeVIC_average19712000_february.asc",format="ascii", overwrite=TRUE)
#writeRaster(mar,"dischargeVIC_average19712000_march.asc",format="ascii", overwrite=TRUE)
#writeRaster(apr,"dischargeVIC_average19712000_april.asc",format="ascii", overwrite=TRUE)
#writeRaster(may,"dischargeVIC_average19712000_may.asc",format="ascii", overwrite=TRUE)
#writeRaster(jun,"dischargeVIC_average19712000_june.asc",format="ascii", overwrite=TRUE)
#writeRaster(jul,"dischargeVIC_average19712000_july.asc",format="ascii", overwrite=TRUE)
#writeRaster(aug,"dischargeVIC_average19712000_august.asc",format="ascii", overwrite=TRUE)
#writeRaster(sep,"dischargeVIC_average19712000_september.asc",format="ascii", overwrite=TRUE)
#writeRaster(oct,"dischargeVIC_average19712000_october.asc",format="ascii", overwrite=TRUE)
#writeRaster(nov,"dischargeVIC_average19712000_november.asc",format="ascii", overwrite=TRUE)
#writeRaster(dec,"dischargeVIC_average19712000_december.asc",format="ascii", overwrite=TRUE)
#writeRaster(annual,"dischargeVIC_average19712000_annual.asc",format="ascii", overwrite=TRUE)

#Marijkes filenames, better 
writeRaster(jan,filename=paste0("Discharge_jan_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(feb,filename=paste0("Discharge_feb_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(mar,filename=paste0("Discharge_mar_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(apr,filename=paste0("Discharge_apr_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(may,filename=paste0("Discharge_may_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(jun,filename=paste0("Discharge_jun_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(jul,filename=paste0("Discharge_jul_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(aug,filename=paste0("Discharge_aug_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(sep,filename=paste0("Discharge_sep_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(oct,filename=paste0("Discharge_oct_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(nov,filename=paste0("Discharge_nov_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(dec,filename=paste0("Discharge_dec_",rcp,".asc"),format="ascii",overwrite=TRUE)

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

writeRaster(jan,filename=paste0("runoff_jan_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(feb,filename=paste0("runoff_feb_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(mar,filename=paste0("runoff_mar_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(apr,filename=paste0("runoff_apr_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(may,filename=paste0("runoff_may_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(jun,filename=paste0("runoff_jun_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(jul,filename=paste0("runoff_jul_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(aug,filename=paste0("runoff_aug_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(sep,filename=paste0("runoff_sep_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(oct,filename=paste0("runoff_oct_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(nov,filename=paste0("runoff_nov_",rcp,".asc"),format="ascii",overwrite=TRUE)
writeRaster(dec,filename=paste0("runoff_dec_",rcp,".asc"),format="ascii",overwrite=TRUE)
