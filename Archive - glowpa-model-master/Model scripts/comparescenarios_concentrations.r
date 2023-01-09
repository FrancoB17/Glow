rm(list=ls(all=TRUE))
configuration <- "standard"

#Set working directory, load libraries, set working path in and out

setwd("D:/Onderzoek/Data & Model/Human/Model scripts/")
library(sp)
library(rworldmap)
library(rgeos)
library(maptools)
library(raster)
library(RColorBrewer)
library(rworldxtra)
working.path.in<-"D:/Onderzoek/Data & Model/Human/Model input/"
working.path.out<-"D:/Onderzoek/Data & Model/Human/Model output/gwpp chapter/"

#Compare country level data

Countries30minNoStates <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep="")))   #has ISO codes going to 904, leading to holes in some parts of Europe, needs fixing. HDI files made based on this file also need fixing
crypto_tot_map <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep="")))
crypto_tot_map_ssp1 <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep="")))
crypto_tot_map_ssp1star <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep="")))
crypto_tot_map_ssp3 <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep="")))
crypto_tot_map$Countries30minNoStates.asc<-NA
crypto_tot_map_ssp1$Countries30minNoStates.asc<-NA
crypto_tot_map_ssp1star$Countries30minNoStates.asc<-NA
crypto_tot_map_ssp3$Countries30minNoStates.asc<-NA

#Fix some numbers that appear on this map erroneously
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 901)  #Normandy
Countries30minNoStates$Countries30minNoStates.asc[a]<-250           #France
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 902)  #Po Plain
Countries30minNoStates$Countries30minNoStates.asc[a]<-380           #Italy
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 903)  #north Spain
Countries30minNoStates$Countries30minNoStates.asc[a]<-724           #Spain

###########2010##############################

presentc<-read.csv(paste(working.path.out,"HumanEmissionsCalculated.csv",sep=""),sep=",",header=TRUE,col.names=c("","countryname","iso","crypto_urb","crypto_rur","crypto_urb_pp","crypto_rur_pp","crypto_urb_under5","crypto_rur_under5","crypto_urb_others","crypto_rur_others","crypto_urb_pp_under5","crypto_rur_pp_under5","crypto_urb_pp_others","crypto_rur_pp_others","crypto_urb_con","crypto_rur_con","crypto_urb_dir","crypto_rur_dir","crypto_urb_dif","crypto_rur_dif","crypto_urb_non","crypto_rur_non"))
presentc$crypto_tot<-presentc$crypto_urb+presentc$crypto_rur

for(i in 1:length(presentc$iso)){
  q<-which(Countries30minNoStates$Countries30minNoStates.asc==presentc$iso[i])
  crypto_tot_map$Countries30minNoStates.asc[q]<-presentc$crypto_tot[i]
}

crypto_tot<-raster(crypto_tot_map)
logcrypto_tot<-log10(crypto_tot)

brks1<-c(10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17)

#geschikt kleurpalet definiëren
colors8<-c("slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4")

#load country border data 
data(countriesHigh)

tiff(paste(working.path.out,"2010Countrybase.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_tot, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE) #xlab="Longitude (degrees)", ylab="Latitude (degrees)",
plot(countriesHigh, add=TRUE)

plot(logcrypto_tot, legend.only=TRUE, col=colors8,breaks=brks1,horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks1, 
                    cex.axis=0.6,font=1.9),
     legend.args=list(text='Cryptosporidium emissions by country (log10 oocysts / year)', font=2.9, cex=0.8)) #side=4, line=2.5, 
dev.off()

###########SSP1##############################

presentc_ssp1<-read.csv(paste(working.path.out,"HumanEmissionsCalculated_ssp1.csv",sep=""),sep=",",header=TRUE,col.names=c("","countryname","iso","crypto_urb","crypto_rur","crypto_urb_pp","crypto_rur_pp","crypto_urb_under5","crypto_rur_under5","crypto_urb_others","crypto_rur_others","crypto_urb_pp_under5","crypto_rur_pp_under5","crypto_urb_pp_others","crypto_rur_pp_others","crypto_urb_con","crypto_rur_con","crypto_urb_dir","crypto_rur_dir","crypto_urb_dif","crypto_rur_dif","crypto_urb_non","crypto_rur_non"))
presentc_ssp1$crypto_tot<-presentc_ssp1$crypto_urb+presentc_ssp1$crypto_rur

for(i in 1:length(presentc_ssp1$iso)){
  q<-which(Countries30minNoStates$Countries30minNoStates.asc==presentc_ssp1$iso[i])
  crypto_tot_map_ssp1$Countries30minNoStates.asc[q]<-presentc_ssp1$crypto_tot[i]
}

crypto_tot_ssp1<-raster(crypto_tot_map_ssp1)
logcrypto_tot_ssp1<-log10(crypto_tot_ssp1)

brks1<-c(10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17)

#geschikt kleurpalet definiëren
colors8<-c("slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4")

#load country border data 
data(countriesHigh)

tiff(paste(working.path.out,"2010Countrybase_ssp1.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_tot_ssp1, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE) #xlab="Longitude (degrees)", ylab="Latitude (degrees)",
plot(countriesHigh, add=TRUE)

plot(logcrypto_tot_ssp1, legend.only=TRUE, col=colors8,breaks=brks1,horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks1, 
                    cex.axis=0.6,font=1.4),
     legend.args=list(text='Cryptosporidium emissions by country (log10 oocysts / year)', font=2.5, cex=0.8)) #side=4, line=2.5, 
dev.off()

###########SSP1star##############################

presentc_ssp1star<-read.csv(paste(working.path.out,"HumanEmissionsCalculated_ssp1star.csv",sep=""),sep=",",header=TRUE,col.names=c("","countryname","iso","crypto_urb","crypto_rur","crypto_urb_pp","crypto_rur_pp","crypto_urb_under5","crypto_rur_under5","crypto_urb_others","crypto_rur_others","crypto_urb_pp_under5","crypto_rur_pp_under5","crypto_urb_pp_others","crypto_rur_pp_others","crypto_urb_con","crypto_rur_con","crypto_urb_dir","crypto_rur_dir","crypto_urb_dif","crypto_rur_dif","crypto_urb_non","crypto_rur_non"))
presentc_ssp1star$crypto_tot<-presentc_ssp1star$crypto_urb+presentc_ssp1star$crypto_rur

for(i in 1:length(presentc_ssp1star$iso)){
  q<-which(Countries30minNoStates$Countries30minNoStates.asc==presentc_ssp1star$iso[i])
  crypto_tot_map_ssp1star$Countries30minNoStates.asc[q]<-presentc_ssp1star$crypto_tot[i]
}

crypto_tot_ssp1star<-raster(crypto_tot_map_ssp1star)
logcrypto_tot_ssp1star<-log10(crypto_tot_ssp1star)

brks1<-c(10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17)

#geschikt kleurpalet definiëren
colors8<-c("slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4")

#load country border data 
data(countriesHigh)

tiff(paste(working.path.out,"2010Countrybase_ssp1star.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_tot_ssp1star, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE) #xlab="Longitude (degrees)", ylab="Latitude (degrees)",
plot(countriesHigh, add=TRUE)

plot(logcrypto_tot_ssp1star, legend.only=TRUE, col=colors8,breaks=brks1,horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks1, 
                    cex.axis=0.6,font=1.4),
     legend.args=list(text='Cryptosporidium emissions by country (log10 oocysts / year)', font=2.5, cex=0.8)) #side=4, line=2.5, 
dev.off()

###########ssp3##############################

presentc_ssp3<-read.csv(paste(working.path.out,"HumanEmissionsCalculated_ssp3.csv",sep=""),sep=",",header=TRUE,col.names=c("","countryname","iso","crypto_urb","crypto_rur","crypto_urb_pp","crypto_rur_pp","crypto_urb_under5","crypto_rur_under5","crypto_urb_others","crypto_rur_others","crypto_urb_pp_under5","crypto_rur_pp_under5","crypto_urb_pp_others","crypto_rur_pp_others","crypto_urb_con","crypto_rur_con","crypto_urb_dir","crypto_rur_dir","crypto_urb_dif","crypto_rur_dif","crypto_urb_non","crypto_rur_non"))
presentc_ssp3$crypto_tot<-presentc_ssp3$crypto_urb+presentc_ssp3$crypto_rur

for(i in 1:length(presentc_ssp3$iso)){
  q<-which(Countries30minNoStates$Countries30minNoStates.asc==presentc_ssp3$iso[i])
  crypto_tot_map_ssp3$Countries30minNoStates.asc[q]<-presentc_ssp3$crypto_tot[i]
}

crypto_tot_ssp3<-raster(crypto_tot_map_ssp3)
logcrypto_tot_ssp3<-log10(crypto_tot_ssp3)

brks1<-c(10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17)

#geschikt kleurpalet definiëren
colors8<-c("slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4")

#load country border data 
data(countriesHigh)

tiff(paste(working.path.out,"2010Countrybase_ssp3.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_tot_ssp3, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE) #xlab="Longitude (degrees)", ylab="Latitude (degrees)",
plot(countriesHigh, add=TRUE)

plot(logcrypto_tot_ssp3, legend.only=TRUE, col=colors8,breaks=brks1,horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks1, 
                    cex.axis=0.6,font=1.4),
     legend.args=list(text='Cryptosporidium emissions by country (log10 oocysts / year)', font=2.5, cex=0.8)) #side=4, line=2.5, 
dev.off()

###########SSP1 DIF##############################

crypto_dif_ssp1<-crypto_tot_ssp1 - crypto_tot

a<-crypto_dif_ssp1<0
b<-crypto_dif_ssp1>0
logcrypto_ssp1dif<-raster(nrows=360,ncols=720)
logcrypto_ssp1dif[a]<--log10(abs(crypto_dif_ssp1[a]))
logcrypto_ssp1dif[b]<-log10(crypto_dif_ssp1[b])

brks1<-c(-17,-16,-15,-14,-1,1,14,15,16,17)
brks2<-c(expression(-10^17),expression(-10^16),expression(-10^15),expression(-10^14),expression(-10^1),expression(10^1),expression(10^14),expression(10^15),expression(10^16),expression(10^17))

#geschikt kleurpalet definieren
colors8<-c("blue4","blue1","deepskyblue","cadetblue2","white","burlywood1","orange1","red1","red4")

#load country border data 
data(countriesHigh)


#Single plot
tiff(paste(working.path.out,"Differencefrom2010_country_ssp1.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_ssp1dif, col=colors8,legend=FALSE, axes=FALSE, breaks=brks1)
plot(countriesHigh, add=TRUE)

plot(logcrypto_ssp1dif, legend.only=TRUE, col=colors8,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks2, 
                    cex.axis=0.7,font=1.9),
     legend.args=list(text='Difference from 2010 by country (log10 oocysts / year)', font=2.9, cex=0.8)) #line=2.5, side=4, 
dev.off()

###########ssp1star DIF##############################

crypto_dif_ssp1star<-crypto_tot_ssp1star - crypto_tot

a<-crypto_dif_ssp1star<0
b<-crypto_dif_ssp1star>0
logcrypto_ssp1stardif<-raster(nrows=360,ncols=720)
logcrypto_ssp1stardif[a]<--log10(abs(crypto_dif_ssp1star[a]))
logcrypto_ssp1stardif[b]<-log10(crypto_dif_ssp1star[b])

brks1<-c(-17,-16,-15,-14,-1,1,14,15,16,17)
brks2<-c(expression(-10^17),expression(-10^16),expression(-10^15),expression(-10^14),expression(-10^1),expression(10^1),expression(10^14),expression(10^15),expression(10^16),expression(10^17))

#geschikt kleurpalet definieren
colors8<-c("blue4","blue1","deepskyblue","cadetblue2","white","burlywood1","orange1","red1","red4")

#load country border data 
data(countriesHigh)


#Single plot
tiff(paste(working.path.out,"Differencefrom2010_country_ssp1star.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_ssp1stardif, col=colors8,legend=FALSE, axes=FALSE, breaks=brks1)
plot(countriesHigh, add=TRUE)

plot(logcrypto_ssp1stardif, legend.only=TRUE, col=colors8,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks2, 
                    cex.axis=0.7,font=1.9),
     legend.args=list(text='Difference from 2010 by country (log10 oocysts / year)', font=2.9, cex=0.8)) #line=2.5, side=4, 
dev.off()

###########ssp3 DIF##############################

crypto_dif_ssp3<-crypto_tot_ssp3 - crypto_tot

a<-crypto_dif_ssp3<0
b<-crypto_dif_ssp3>0
logcrypto_ssp3dif<-raster(nrows=360,ncols=720)
logcrypto_ssp3dif[a]<--log10(abs(crypto_dif_ssp3[a]))
logcrypto_ssp3dif[b]<-log10(crypto_dif_ssp3[b])

brks1<-c(-17,-16,-15,-14,-1,1,14,15,16,17)
brks2<-c(expression(-10^17),expression(-10^16),expression(-10^15),expression(-10^14),expression(-10^1),expression(10^1),expression(10^14),expression(10^15),expression(10^16),expression(10^17))

#geschikt kleurpalet definieren
colors8<-c("blue4","blue1","deepskyblue","cadetblue2","white","burlywood1","orange1","red1","red4")

#load country border data 
data(countriesHigh)


#Single plot
tiff(paste(working.path.out,"Differencefrom2010_country_ssp3.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logcrypto_ssp3dif, col=colors8,legend=FALSE, axes=FALSE, breaks=brks1)
plot(countriesHigh, add=TRUE)

plot(logcrypto_ssp3dif, legend.only=TRUE, col=colors8,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks2, 
                    cex.axis=0.7,font=1.9),
     legend.args=list(text='Difference from 2010 by country (log10 oocysts / year)', font=2.9, cex=0.8)) #line=2.5, side=4, 
dev.off()

###############################################################################################################

#Compare grid data

present<-raster(paste0(working.path.out,"rota.asc"))
ssp1<-raster(paste0(working.path.out,"rota_scen1.asc"))
#ssp1star<-raster(paste0(working.path.in,"rota_ssp1star.grd"))
ssp3<-raster(paste0(working.path.out,"rota_scen2.asc"))

difssp1<-ssp1-present
#difssp1star<-ssp1star-present
difssp3<-ssp3-present

incrssp1<-ssp1/present
incrssp3<-ssp3/present

#nu het verschil omgezet in log -> alles groter dan 0 kan gewoon log, alles 0 blijft 0 en
#alles kleiner dan 0 moet positief worden en dan log, maar wel zorgen dat NA/missing niet meegenomen wordt.

a<-difssp1<0
#b<-difssp1star<0
c<-difssp3<0

q<-difssp1>0
#r<-difssp1star>0
s<-difssp3>0


logdifssp1<-raster(nrows=360,ncols=720)
logdifssp1[q]<-log10(difssp1[q])
logdifssp1[a]<--log10(abs(difssp1[a]))

#logdifssp1star<-raster(nrows=360,ncols=720)
#logdifssp1star[r]<-log10(difssp1star[r])
#logdifssp1star[b]<--log10(abs(difssp1star[b]))

logdifssp3<-raster(nrows=360,ncols=720)
logdifssp3[s]<-log10(difssp3[s])
logdifssp3[c]<--log10(abs(difssp3[c]))

logincrssp1<-log10(incrssp1)
logincrssp3<-log10(incrssp3)


#breaks definieren om als label bij te kunnen voegen
brks1<-c(-17,-15,-13,-10,-6,6,10,13,15,17)
brks2<-c(expression(-10^17),expression(-10^15),expression(-10^13),expression(-10^10),expression(-10^6),expression(10^6),expression(10^10),expression(10^13),expression(10^15),expression(10^17))

brks3<-c(-1,0,0.2,0.4,0.6,0.8,1.2,1.4,1.6,1.8,2)

#geschikt kleurpalet definieren
colors8<-c("blue4","blue1","deepskyblue","cadetblue2","white","burlywood1","orange1","red1","red4")
colors82<-c("blue4","blue1","deepskyblue","cadetblue2","white","burlywood1","orange1","red1","red4")

#load country border data 
data(countriesHigh)


#Single plot
tiff(paste(working.path.out,"Rota_Differencefrom2010_scen1.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=300) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logdifssp1, col=colors8,legend=FALSE, axes=FALSE, breaks=brks1)
plot(countriesHigh, add=TRUE)

plot(logdifssp1, legend.only=TRUE, col=colors8,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks2, 
                    cex.axis=1,font=2),
     legend.args=list(text='Difference from 2010 (log10 viral particles / grid / year)', font=2, cex=1.2)) #line=2.5, side=4, 
dev.off()

##Single plot
#tiff(paste(working.path.out,"Differencefrom2010_ssp1star.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=300) #put resolution at 300 for high quality image for publication
#par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
#plot(logdifssp1star, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE)
#plot(countriesHigh, add=TRUE)
#
#plot(logdifssp1star, legend.only=TRUE, col=colors8,breaks=brks1, horizontal=TRUE,
#     legend.width=1, legend.shrink=0.75,
#     axis.args=list(at=brks1,
#                    labels=brks1, 
#                    cex.axis=0.6,font=1.4),
#     legend.args=list(text='Difference from present (log10 oocysts / grid / year)', font=1.8, cex=0.8))
#dev.off()
#
#Single plot
tiff(paste(working.path.out,"Rota_Differencefrom2010_scen2.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=300) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logdifssp3, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE)
plot(countriesHigh, add=TRUE)

plot(logdifssp3, legend.only=TRUE, col=colors8,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks2, 
                    cex.axis=1,font=2),
     legend.args=list(text='Difference from present (log10 viral particles / grid / year)', font=2, cex=1.2))
dev.off()

#Single plot
tiff(paste(working.path.out,"Rota_incrfrom2010_scen1.tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=300) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logincrssp1, col=colors82,legend=FALSE, axes=FALSE, breaks=brks3)
plot(countriesHigh, add=TRUE)

plot(logincrssp1, legend.only=TRUE, col=colors82,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks3,
                    labels=brks3, 
                    cex.axis=0.6,font=1.4),
     legend.args=list(text='Increase from 2010 (log10 oocysts / viral particles / year)', font=1.8, cex=0.8)) #line=2.5, side=4, 
dev.off()
