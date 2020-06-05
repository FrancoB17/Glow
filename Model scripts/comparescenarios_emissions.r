
#Compare grid data

present<-raster(paste0(working.path.out,"humanemissions_Agroknow",pathogen,"1.tif"))
scenario<-raster(paste0(working.path.out,"humanemissions_Agroknow",pathogen,run,".tif"))
present[is.na(present)]<-0
scenario[is.na(scenario)]<-0

difscenario<-scenario-present

#nu het verschil omgezet in log -> alles groter dan 0 kan gewoon log, alles 0 blijft 0 en
#alles kleiner dan 0 moet positief worden en dan log, maar wel zorgen dat NA/missing niet meegenomen wordt.

a<-which(difscenario[]<0)
q<-which(difscenario[]>0)

logdifscenario<-present
logdifscenario[]<-0
logdifscenario[q]<-log10(difscenario[q])
logdifscenario[a]<--log10(abs(difscenario[a]))

#Make .png plot

colors<-c("blue4","blue1","deepskyblue","cadetblue2","white","burlywood1","orange1","red1","red4")

mx1<-max(logdifscenario[q],na.rm=TRUE)
mx2<-min(logdifscenario[a],na.rm=TRUE)
mx<-max(mx1,mx2)
brks<-c(-ceiling(mx),-ceiling(mx)+2,-ceiling(mx)+4,-ceiling(mx)+6,-4,4,ceiling(mx)-6,ceiling(mx)-4,ceiling(mx)-2,ceiling(mx))
brks2<-c(-1*10^(-brks[1]),-1*10^(-brks[2]),-1*10^(-brks[3]),-1*10^(-brks[4]),-1*10^(-brks[5]),1*10^(brks[6]),1*10^(brks[7]),1*10^(brks[8]),1*10^(brks[9]),1*10^(brks[10]))
#brks2<-c(expression(-10^as.character(ceiling(mx))),expression(-10^as.character(ceiling(mx)-2)),expression(-10^as.character(ceiling(mx)-4)),expression(-10^as.character(ceiling(mx)-6)),expression(-10^4),expression(10^4),expression(10^as.character(ceiling(mx)-6)),expression(10^as.character(ceiling(mx)-4)),expression(10^as.character(ceiling(mx)-4)),expression(10^as.character(ceiling(mx)-2)))
#brks2<-c(expression(-10^as.character(ceiling(mx))),expression(-10^as.character(ceiling(mx)-2)),expression(-10^as.character(ceiling(mx)-4)),expression(-10^as.character(ceiling(mx)-6)),expression(-10^4),expression(10^4),expression(10^as.character(ceiling(mx)-6)),expression(10^as.character(ceiling(mx)-4)),expression(10^as.character(ceiling(mx)-4)),expression(10^as.character(ceiling(mx)-2)))

#plot Png file
filename_png<-paste0(working.path.out,"difference_",pathogen,".png")
png(filename=filename_png,width=750,height=750,units="px")

par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n") 
plot(logdifscenario, col=colors, breaks=brks,legend=FALSE, axes=FALSE)
title(main=paste0("Difference between baseline and scenario"))  

#if the resolution is 0.08333 or 0.008333, the district data or higher res need to be plotted, if the resolution is 0.5, the country data needs to be plotted
if(resolution == 0.5){
  plot(countriesHigh, add=TRUE)
}else{
  setwd(paste0(working.path.in,"districts_shapefile/"))
  plot(readOGR(dsn = ".", layer = overall_inputs$shapefile_filename),add=TRUE)
  setwd<-wd
}

plot(logdifscenario, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks,
                    labels=brks2, 
                    cex.axis=1,font=1.4),
     legend.args=list(text="Difference from baseline (viral particles / year)", font=1.8, cex=1.2)) 

dev.off()

#end of plotting .png file

