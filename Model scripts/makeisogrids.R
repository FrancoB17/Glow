#script is written by Nynke 15-5-2019. It works for the Uganda rasters. No clue if it also works for other areas in the world.

setwd(paste0(working.path.in,"polygons/"))
area <- readOGR(dsn = ".", layer = layer)

a<-extent(area)

#number of rows and cols: 

lon1<-round(a[1],digits=1)-(resolution*12)
lon2<-round(a[2],digits=1)+(resolution*12)
lat1<-round(a[3],digits=1)
lat2<-round(a[4],digits=1)+(resolution*12)

rows<-(lat2-lat1)/resolution
cols<-(lon2-lon1)/resolution

rast<-matrix(data=NA,nrow=rows,ncol=cols)
rast<-raster(rast)

extent(rast) <-c(lon1,lon2,lat1,lat2)
projection(rast) <- CRS("+proj=longlat +datum=WGS84")

area_rast<-rasterize(area,rast,"OBJECTID")

areas<-data.frame(area$OBJECTID,area$DNAME2014)

write.csv(areas, file=paste0(working.path.in,"polygons/isoareas.csv"))
writeRaster(area_rast,paste0(working.path.in,"isoraster.asc"),overwrite=TRUE)

rm(c(area,a,lon1,lon2,lat1,lat2,rows,cols,rast,area_rast))
