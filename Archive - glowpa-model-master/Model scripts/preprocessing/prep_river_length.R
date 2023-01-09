# This script calculates river length, defined as midpoint distance between two gridcells
# adjusted from Marijkes work by Lucie
#moved to preprocessing on 28-6-2017

river_dir <- "D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/"
mons <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
monfull <- c("january","february","march","april","may","june","july","august","september","october","november","december")
flowdir<- t(as.matrix(readAsciiGrid(paste0(river_dir,"FlowDirectionDDM30preprocessed.asc"))))

mon_days <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
mon_sec <- 2628000 #number of seconds in 365-day-year divided by 12

### calculate river length

lats <- seq(89.75,-89.75,-0.5)
longs <- seq(-179.75,179.75,0.5)

flowdist <- matrix(NA,360,720)

#compute length of river stretch in meters, using the function pointDistance
#depending on flow direction

for(h in 1:360)
{
  for(j in 1:720)
  {
    tmp <- flowdir[h,j]
    if(is.na(tmp)) {next}    
    
    lat <- lats[h]
    lon <- longs[j]
    
    #Marijke had de plussen en minnen andersom bij up en down
    #dit leek mij logischer
    if(is.element(tmp, c(1,16)))        {flowdist[h,j]<-pointDistance(c(lon,lat),c(longs[j+1],lat),lonlat=TRUE)} #horizontal
    if(tmp==4)                          {flowdist[h,j]<-pointDistance(c(lon,lat),c(lon,lats[h-1]),lonlat=TRUE)} #down
    if(tmp==64)                         {flowdist[h,j]<-pointDistance(c(lon,lat),c(lon,lats[h+1]),lonlat=TRUE)} #up
    if(is.element(tmp, c(2,8)))         {flowdist[h,j]<-pointDistance(c(lon,lat),c(longs[j+1],lats[h-1]),lonlat=TRUE)} #down diagonal
    if(is.element(tmp, c(32,128)))      {flowdist[h,j]<-pointDistance(c(lon,lat),c(longs[j+1],lats[h+1]),lonlat=TRUE)} #up diagonal
    if(tmp==9)                          {flowdist[h,j]<-NA} # outflow = no distance  
  }
}

#give the outflow cells with code 9 the mean flowdist of surrounding cells
for(h in 1:360)
{
  for(j in 1:720)
  {
    tmp <- flowdir[h,j]
    if(is.na(tmp)) {next}    
    
    if(tmp==9 && h>1 && h<360 && j>1 && j<720)
    {flowdist[h,j]<-mean(c(flowdist[h-1,j-1],
                           flowdist[h-1,j],
                           flowdist[h-1,j+1],
                           flowdist[h,j+1],
                           flowdist[h+1,j+1],
                           flowdist[h+1,j],
                           flowdist[h+1,j-1],
                           flowdist[h,j-1]), na.rm=TRUE)} 
  }
}
check<-raster(flowdist)
plot(check)

#give the remaining ones at the edges of the raster the mean flowdist of the latitude
for(h in 1:360)
{
  for(j in 1:720)
  {
    tmp <- flowdist[h,j]
    if(is.na(tmp)) {next}    
    
    lat <- lats[h]
    lon <- longs[j]
    
    if(tmp==1000000)
    {flowdist[h,j]<-mean(c(flowdist[h,]), na.rm=TRUE)} 
  }
}

check<-raster(flowdist)
plot(check)

#waar komen de enkele hele hoge waardes in groenland en noord canada vandaan?

river_length <- raster(flowdist,xmn=-180,xmx=180,ymn=-90,ymx=90,crs=NA, template=NULL)
#plot(river_length)
writeRaster(river_length,filename=paste0("river_length.asc"),format="ascii",overwrite=TRUE)
