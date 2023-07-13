rm(list = ls())

a <- raster("D:/MST/WUR/OneDrive - Wageningen University & Research/PhD/RQ1 E. coli model/GloWPa-E1/Temp/chinapop01.tif")

plot(log(a))

library(raster)
library(rgdal)
library(maps)
library(mapdata)

nruns<-1  #25

for(i in 1:nruns){
  
  run<-i
  run<-1
  plotting_path_in<- "D:/MST/Test_GloWPa/glowpa-model/Model output/"

  file <- sprintf("%shumanemissions_ecoli_%s.tif",plotting_path_in,run)
  rasterfile <- raster(file)
  logfile<-rasterfile
  a<-which(rasterfile[]>0) #to make sure that values of 0 do not become -inf
  logfile[a] <- log10(rasterfile[a])
                                
  fname <- sprintf("humanemissions_ecoli_%s.png",run)
  out_file <- paste0(plotting_path_in,fname)
  
  #this is where you need to play around with what fits best to the data. Have a look at what we used for the paper, for example, and make sure your maximum
  #emissions do not exceed the last break. The number of colors is always one less than the breaks
  col <- c("white","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","blue","blue","blue","skyblue","limegreen", "yellow", "orange","darkorange","red","red2")
  breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  # width <- 750
  # height <- 400
  
  #png(filename = out_file, width = width, height = height, units = "px") #you may not want to use px for a publication, but it should be ok for now
  par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n",bg="white")
  plot(logfile,col=col,breaks=breaks,legend=FALSE,axes=FALSE,ext=extent(logfile))
  plot(logfile,col=col,axes=FALSE)
  plot(logfile,col=col,breaks=breaks,main="E. coli load to rivers from human source(log)")
  #boundaries<-readOGR(dsn="C:/Users/hofst023/OneDrive - Wageningen University & Research/Oude D schijf/Onderzoek/K2P/test/gadm36_global/gadm36.gpkg",layer="") 
  #plot(boundaries)
  map('worldHires',add=TRUE)
  
  unit <- "(log10 cells / year)"
  
  legend_text <- sprintf("E. coli emissions %s",unit)
  labels <- breaks
  plot(logfile, legend.only=TRUE, col=col,breaks=breaks,horizontal=TRUE,
  legend.width=0.5, legend.shrink=0.4,
  axis.args=list(at=breaks,
              labels=labels,
              cex.axis=1,font=1.4),
  legend.args=list(text=legend_text, font=1.8, cex=1))
  
  dev.off()

}

