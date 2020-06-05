#This script is prepared by Nynke, June 2019
#The script prepares the Uganda hydrological data from IIASA for use in GloWPa
#Input are NetCDF files on discharge, runoff, water temperature, water level, short wave irradiance
#and also a tiff file for waterdistance and an ldd file for drain direction

#The model produces 32 year average monthly stacks and saves those in the right folder for GloWPa
rm(list=ls(all=TRUE))

#first list the .nc files in the directory
library(ncdf4)
library(raster)
library(dplyr)

##Set working directory
wd <- setwd("D:/Onderzoek/K2P/hydrology data/")
path <- wd
outpaths<-(c("D:/Onderzoek/K2P/GloWPa model/Model input/Uganda/discharge/","D:/Onderzoek/K2P/hydrology data/output/","D:/Onderzoek/K2P/GloWPa model/Model input/Uganda/SWradiation/","D:/Onderzoek/K2P/GloWPa model/Model input/Uganda/surfacerunoff/","D:/Onderzoek/K2P/hydrology data/output/","D:/Onderzoek/K2P/hydrology data/output/", "D:/Onderzoek/K2P/GloWPa model/Model input/Uganda/watertemperature/"))
GFDL_list <- list.files(path = path, pattern = "\\.(nc|NC|Nc)$")
variable<-c("discharge_monthavg","Precipitation_monthavg", "Rsds_monthavg", "runoff_monthavg", "Tavg_monthavg", "waterLevel_monthavg", "waterTemperature_monthavg")
variable1<-c("discharge","precipitation","SWdown_mean","surfacerunoff","Tavg","waterlevel","Tw")

isoraster<-raster("D:/Onderzoek/K2P/GloWPa model/Model input/Uganda/isoraster_Uganda.asc") #now read the isogrids

GFDL_list <- lapply(GFDL_list, nc_open)

GFDL_r_list <- list() #Empty lists for for-loop

#Extract descriptive and variable data
for (i in 1:length(GFDL_list)){
  lon <- ncvar_get(GFDL_list[[i]], var = "lon")
  lat <- ncvar_get(GFDL_list[[i]], var = "lat")
  time <- ncvar_get(GFDL_list[[i]], var = "time")

  data <- ncvar_get(GFDL_list[[i]], var = variable[i])
  
  #Turn into raster
  dims <- GFDL_list[[i]]$dim #NCDF dimension attributes
  extent<-extent(c(min(dims$lat$vals)-(0.08333333/2),max(dims$lat$vals)+(0.08333333/2),min(dims$lon$vals)-(0.08333333/2),max(dims$lon$vals)+(0.08333333/2)))
  data_r <- brick(data) #Make brick from "tas" variable
  extent(data_r) <- extent #Set extent
  data_r<-t(data_r) #transpose data
  
  #Make new rasterbricks with 12 layers (mean of each month over 40 years) and store in list
  GFDL_m_r <- stackApply(data_r, indices = c(1:12), fun = mean, na.rm = T)
  GFDL_m_r<-crop(GFDL_m_r,extent(isoraster))
  
  #if i=2 or i=4 -> transform units from m to mm/day => x1000
  if(i==2 | i==4){
    GFDL_m_r<-1000*GFDL_m_r
  }

  #Save in appropriate location
  writeRaster(GFDL_m_r[[1]],paste0(outpaths[i],variable1[i],"_jan_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[2]],paste0(outpaths[i],variable1[i],"_feb_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[3]],paste0(outpaths[i],variable1[i],"_mar_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[4]],paste0(outpaths[i],variable1[i],"_apr_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[5]],paste0(outpaths[i],variable1[i],"_may_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[6]],paste0(outpaths[i],variable1[i],"_jun_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[7]],paste0(outpaths[i],variable1[i],"_jul_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[8]],paste0(outpaths[i],variable1[i],"_aug_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[9]],paste0(outpaths[i],variable1[i],"_sep_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[10]],paste0(outpaths[i],variable1[i],"_oct_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[11]],paste0(outpaths[i],variable1[i],"_nov_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  writeRaster(GFDL_m_r[[12]],paste0(outpaths[i],variable1[i],"_dec_1"), format="ascii", overwrite=TRUE,NAflag=-9999)
  
  print(i)
}