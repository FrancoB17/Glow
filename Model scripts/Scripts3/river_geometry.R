#This script calculates river width, depth and flow velocity from discharge
# then we calculate residence time in a cell

# it is based on the assumptions in Van Vliet et al. 2012
# adjusted from Marijkes work by Lucie

# Adjusted by Nynke in april 2019 to enable running more flexibly

river_dir <- working.path.in
mons <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
flowdir<- t(as.matrix(readAsciiGrid(paste0(river_dir,overall_inputs$flowdirection_filename[i]))))

mon_days <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
mon_sec <- 2628000 #number of seconds in 365-day-year divided by 12

setwd(paste0(working.path.in,"rivergeometry"))
river_length <- raster("river_length.asc")
river_length<-river_length*overall_inputs$length[i]

#river depth function
depth <- function(dis)
{  
  dmatrix <- 0.34*dis^0.341
  
  return(dmatrix)
}

#river width function
width <- function(dis)
{
  wmatrix <- 1.22*dis^0.557
}
 
# for all 12 months
for(h in 1:12)
{
    discharge<-raster(paste0(working.path.in,"discharge/discharge_",mons[h],"_",i,".asc")) #in m3/s
    discharge<-discharge*overall_inputs$discharge[i]
    discharge<-crop(discharge,extent(isoraster_hydro))
   
    river_depth <- depth(discharge) #in m
    river_width <- width(discharge) #in m
    river_crossection  <- river_depth * river_width # in m2
    river_flowvelocity <- discharge/river_crossection # in m/s
    res <- river_length/river_flowvelocity # residence time in seconds
    res_mon <- res/mon_sec #residence time in months

    setwd(paste0(working.path.in,"rivergeometry"))
    writeRaster(river_depth,filename=paste0("river_depth_",i,mons[h],".asc"),format="ascii",overwrite=TRUE)
    writeRaster(river_width,filename=paste0("river_width_",i,mons[h],".asc"),format="ascii",overwrite=TRUE)
    writeRaster(river_flowvelocity,filename=paste0("river_flowvelocity_",i,mons[h],".asc"),format="ascii",overwrite=TRUE)
    writeRaster(res_mon,filename=paste0("river_restime_",i,mons[h],".asc"),format="ascii",overwrite=TRUE)
   
    #print(h)
  }
rm(discharge,river_crossection,river_depth,river_width,river_flowvelocity,river_length)
rm(res,res_mon,width,depth,flowdir)

#check out values
#secs2days<- 60*60*24
#res_day<-res/secs2days
#plot(res_day)
#cellStats(res_day, stat="max")

# this residence time may be too fast, as rivers are assumed to flow in straight lines from middle to middle of cells. 
#e.g. using hte formula above, if the river length is higher, then the residence time will also be higher

# note from Schulze 2005 et al.:
# the assumption of a rectangular cross section is considered
#as acceptable for bankful discharge. For less than bankfull
#discharge, which is the normal case, width and depth are
#scaled, but their ratio remains the same. Under natural conditions,
#in a flat and broad river bed, depth would decrease
#faster than width with falling discharge. Hence the model
#tends to overestimate the ratio of depth to width. As the hydraulic
#radius and thus river velocity are especially sensitive
#to changes in depth with this first approach velocity results
#for less than bankful discharge will be overestimated which
#has to be kept in mind regarding the results.
