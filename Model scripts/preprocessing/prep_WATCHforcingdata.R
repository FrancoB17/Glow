### 12 december 2014
### Marijke van Hengel
### This script preprocesses the EU-WATCH climate data. 
### Variables in the script are currently named after Short wave radiation, 
# but it works for the other data as well

library(ncdf4) # necessary to process .nc4 files
library(raster)

### Settings
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/UV/SWradiation_WATCHforcing/raw") #working directory
output_dir <-"D:/LuciePhD/Model/Cryptomodel/modeldata/UV/SWradiation_WATCHforcing/processed" #output directory

filename <- "SWdown_monthly_WFD_grid_1958-2001.nc" #file to process

### open netcdf file
file <- nc_open(filename, write=FALSE, readunlim=FALSE, verbose=FALSE, suppress_dimvals=FALSE) #nc_open statement, needed for further processing

# to use on WATCH monthly data, start here
# for using WATCH daily data, scroll down

#idee: creer gewoon 528 rasters, voor elke maand, ga daarna die middelen
mon<-c(1,2,3,4,5,6,7,8,9,10,11,12)
mon528<-rep(c(1,2,3,4,5,6,7,8,9,10,11,12),44)
mons <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
mons528<-rep(c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),44)
years<-c(1958:2001)
years528<-rep(years,each=12)
  
setwd(output_dir)
for (i in 1:528){
var1<-ncvar_get(file,varid="SWdown",start=c(1,1,i),count=c(-1,-1,1))
varmatrix<-apply(t(as.matrix(var1)), c(1,2), rev)
varraster<-raster(nrows=360,ncols=720)
varraster[]<-varmatrix
writeRaster(varraster,filename=paste0("SWdown_",years528[i], "_",mons528[i],".asc"),format='ascii',overwrite=TRUE)
print(i)
}

# I need to make means of the years 1971-2000
#the unit is W/m2 (mean daily total solar irradiance)
#King et al. is in Kj/m2 -> time component
years<-c(1971:2000)
for(i in 1:12){
print(i)
filenames<-paste0("SWdown_",years,"_",mons[i],".asc")
s<-stack(filenames)
smean<-calc(s,fun=mean)
plot(smean,main=paste0("Mean shortwave radiation ", mons[i]))
writeRaster(smean,filename=paste0("SWdown_mean1971-2000_",mons[i],".asc"),format='ascii',overwrite=TRUE)
}

# apply the function to the list of netcdf files
rasterlist<-lapply(mylist,makeUVraster)












# in case we want to process daily data, this bit is needed
# if processing WATCH monthly data, this can be skipped
# these are currently named after Tair
day_month_start <- c(1,32,60,91,121,152,182,213,244,274,305,335) #start days of each month, regular year
day_month_start_leap <- c(1,32,61,92,122,153,183,214,245,275,306,336) #start days of each month, leap year

#################
## This function computes monthly averages for a given month of daily scale data given the day that months starts (depends on leap/normal year).
monthly_Tair <- function(dailyTair,daynumber,meanT)
{
  meanT=apply(dailyTair,c(1,2),mean)
  return(meanT)
}
####################################

####################################
## This boolean function returns for a given first day of a year since 1958 whether it is a leap year (TRUE) or not (FALSE)
leap_year <- function(yearstart)
{
  leapyears <- c(5113,6574,8035,9496,10957,12418,13879)
  if(is.element(yearstart,leapyears)) return(TRUE)
  else return(FALSE)
}
###################################


###################################
## This recursive function takes as input a month number, the start day since 1-1-1958 of the 30 year period one is interested in, and the average so far (0 in the first call).
# It then computes the average month in that 30 year period. 
# It needs the leap year check function and the monthly_Tair function
clim_monthly_Tair <- function(month,yearstart,avg_T)
{
  if(leap_year(yearstart)) # case leap year
  {
    startDay <- yearstart + day_month_start_leap[month] - 1
    dat <- ncvar_get(file,varid="Tair",start=c(1,1,startDay),count=c(-1,-1,28))
    avg_T <- avg_T + 1/30 * monthly_Tair(dat,startDay,0)
    yearstart <- yearstart+366    
  }
    else # case not a leap year
    {
      startDay <- yearstart+day_month_start[month]-1
      dat <- ncvar_get(file,varid="Tair",start=c(1,1,startDay),count=c(-1,-1,28))
      avg_T <- avg_T + 1/30 * monthly_Tair(dat,startDay,0)
      yearstart <- yearstart+365                 
    }
  
  if(yearstart<15340) {clim_monthly_Tair(month,yearstart,avg_T)}
    else{return(avg_T)}
}
###################################

# Process the data, transpose it (since lon/lat are apparently switched in EU-WATCH data)
# WARNING: THIS PART IS VERY SLOW. IT TAKES ABOUT 5 MINUTES TO RUN 1 MONTH. FOR DEBUGGING, 1 MONTH IS ENOUGH.

print(0)
clim_T_jan <- t(clim_monthly_Tair(1,4383,0))
writeRaster(raster(clim_T_jan,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_jan.asc",format="ascii",overwrite=TRUE)
rm(clim_T_jan)
print(1)
clim_T_feb <- t(clim_monthly_Tair(2,4383,0))
writeRaster(raster(clim_T_feb,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_feb.asc",format="ascii",overwrite=TRUE)
rm(clim_T_feb)
print(2)
clim_T_mar <- t(clim_monthly_Tair(3,4383,0))
writeRaster(raster(clim_T_mar,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_mar.asc",format="ascii",overwrite=TRUE)
rm(clim_T_mar)
print(3)
clim_T_apr <- t(clim_monthly_Tair(4,4383,0))
writeRaster(raster(clim_T_apr,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_apr.asc",format="ascii",overwrite=TRUE)
rm(clim_T_apr)
print(4)
clim_T_may <- t(clim_monthly_Tair(5,4383,0))
writeRaster(raster(clim_T_may,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_may.asc",format="ascii",overwrite=TRUE)
rm(clim_T_may)
print(5)
clim_T_jun <- t(clim_monthly_Tair(6,4383,0))
writeRaster(raster(clim_T_jun,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_jun.asc",format="ascii",overwrite=TRUE)
rm(clim_T_jun)
print(6)
clim_T_jul <- t(clim_monthly_Tair(7,4383,0))
writeRaster(raster(clim_T_jul,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_jul.asc",format="ascii",overwrite=TRUE)
rm(clim_T_jul)
print(7)
clim_T_aug <- t(clim_monthly_Tair(8,4383,0))
writeRaster(raster(clim_T_aug,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_aug.asc",format="ascii",overwrite=TRUE)
rm(clim_T_aug)
print(8)
clim_T_sep <- t(clim_monthly_Tair(9,4383,0))
writeRaster(raster(clim_T_sep,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_sep.asc",format="ascii",overwrite=TRUE)
rm(clim_T_sep)
print(9)
clim_T_oct <- t(clim_monthly_Tair(10,4383,0))
writeRaster(raster(clim_T_oct,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_oct.asc",format="ascii",overwrite=TRUE)
rm(clim_T_oct)
print(10)
clim_T_nov <- t(clim_monthly_Tair(11,4383,0))
writeRaster(raster(clim_T_nov,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_nov.asc",format="ascii",overwrite=TRUE)
rm(clim_T_nov)
print(11)
clim_T_dec <- t(clim_monthly_Tair(12,4383,0))
writeRaster(raster(clim_T_dec,-180,180,-90,90),filename="D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/1970_2000_Tair_dec.asc",format="ascii",overwrite=TRUE)
print(12)

#uncomment if you want to plot
plot(raster(clim_T_dec,xmn=0.25,xmx=359.75,ymn=-89.75,ymx=89.75),col=topo.colors(250),main="1970-2000 average temperature December")

nc_close(file) #close the .nc file. Do not forget, or RAM will get full!
setwd("D:/LuciePhD/Model/Cryptomodel/temporary")
