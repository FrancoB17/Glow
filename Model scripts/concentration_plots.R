# this script makes some extra plots of the oocyst concentrations
# this script can be run after running the whole model including the routing
# made in June 2017 by Lucie

i<-1 #I want to do only the baseline run

#### extra colors

colorsload<-c("blue", "blue","dodgerblue", "green", "yellow", "orange","red", "red4", "red4")
colorsconc<-c("blue", "dodgerblue","green", "yellow", "orange","orange","red","red", "red4", "red4")
colors3<-c("lightblue","palegreen3", "yellow", "orange", "red","red4")
colors3b<-c("lightblue","palegreen3", "yellow", "orange", "red")
colors10<-colorRampPalette(c("blue","blue", "blue", "blue","dodgerblue", "dodgerblue","dodgerblue","lightblue", "green3", "yellow","darkorange", "red1", "purple","black"))(99) #"purple"
colors11<-colorRampPalette(c("blue","blue", "blue", "blue","dodgerblue", "dodgerblue","dodgerblue","lightblue", "green3", "gold", "darkorange","red1", "purple","black"))(99)


#### regional analysis ########

#reading in region raster
regionraster<- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/regionraster.asc")
regioncodes<-read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/regioncodes.csv")
regioncodes$jan<-NA
regioncodes$feb<-NA
regioncodes$mar<-NA
regioncodes$apr<-NA
regioncodes$may<-NA
regioncodes$jun<-NA
regioncodes$jul<-NA
regioncodes$aug<-NA
regioncodes$sep<-NA
regioncodes$oct<-NA
regioncodes$nov<-NA
regioncodes$dec<-NA

#define regions
africa<-regionraster == 1
asia<-regionraster==2
europe<-regionraster==3
latinamerica<-regionraster==4
mena<-regionraster==5
northamerica<-regionraster==6
oceania<-regionraster==7

mons <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
regiondf<-data.frame(mons)
working.path.data<-"D:/LuciePhD/Model/Cryptomodel/modeldata/oocysts/"

regiondf$africa_conc_mean<-NA
regiondf$asia_conc_mean<-NA
regiondf$europe_conc_mean<-NA
regiondf$latinamerica_conc_mean<-NA
regiondf$mena_conc_mean<-NA
regiondf$northamerica_conc_mean<-NA
regiondf$oceania_conc_mean<-NA

regiondf$africa_conc_median<-NA
regiondf$asia_conc_median<-NA
regiondf$europe_conc_median<-NA
regiondf$latinamerica_conc_median<-NA
regiondf$mena_conc_median<-NA
regiondf$northamerica_conc_median<-NA
regiondf$oceania_conc_median<-NA

regiondf$africa_load_mean<-NA
regiondf$asia_load_mean<-NA
regiondf$europe_load_mean<-NA
regiondf$latinamerica_load_mean<-NA
regiondf$mena_load_mean<-NA
regiondf$northamerica_load_mean<-NA
regiondf$oceania_load_mean<-NA

regiondf$africa_load_median<-NA
regiondf$asia_load_median<-NA
regiondf$europe_load_median<-NA
regiondf$latinamerica_load_median<-NA
regiondf$mena_load_median<-NA
regiondf$northamerica_load_median<-NA
regiondf$oceania_load_median<-NA

regiondf$africa_f_diffuse<-NA
regiondf$asia_f_diffuse<-NA
regiondf$europe_f_diffuse<-NA
regiondf$latinamerica_f_diffuse<-NA
regiondf$mena_f_diffuse<-NA
regiondf$northamerica_f_diffuse<-NA
regiondf$oceania_f_diffuse<-NA

out_dir <- "D:/LuciePhD/Model/Cryptomodel/modeloutput/"
setwd(out_dir)
dis_mask<-raster(paste0("D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/discharge_mask/dismask_200m3s.asc"))

######## regional analysis plots ##############

#for all 12 months
for(k in 1:12){
  
  conc <- raster(paste0("total_oocyst_concentration_",mons[k],"1.asc"))
  conc[conc==Inf]<-NA
  load <- raster(paste0("total_oocyst_load_",mons[k],"1.asc"))
  load_cor<-round(load,digits=0)
  Diffuse<- raster(paste0(working.path.data,"diffuse_streaminput_1",mons[k],".asc"))
  Diffuse[is.na(Diffuse)] <- 0
  Point<-raster("D:/LuciePhD/Model/Cryptomodel/modeloutput_humans/humanemissions.asc")/12 #divide by 12 for monthly
  Point[is.na(Point)] <- 0
  
  #who category plot
  who<-conc
  who[who<=5]<-1
  who[who>5 & who<=50]<-2
  who[who>50 & who<=500]<-3
  who[who>500 & who<=5000]<-4
  who[who>5000 & who<=50000]<-5
  who[who>50000]<-6

  #create table with statistics of the concentrations and loads per region
  regiondf$africa_conc_mean[k]<-mean(conc[africa],na.rm=TRUE)
  regiondf$africa_conc_median[k]<-median(conc[africa],na.rm=TRUE)
  regiondf$africa_load_mean[k]<-mean(load_cor[africa],na.rm=TRUE)
  regiondf$africa_load_median[k]<-median(load_cor[africa],na.rm=TRUE)
  regiondf$africa_f_diffuse[k]<-sum(Diffuse[africa])/(sum(Diffuse[africa])+sum(Point[africa]))
  
  regiondf$asia_conc_mean[k]<-mean(conc[asia],na.rm=TRUE)
  regiondf$asia_conc_median[k]<-median(conc[asia],na.rm=TRUE)
  regiondf$asia_load_mean[k]<-mean(load_cor[asia],na.rm=TRUE)
  regiondf$asia_load_median[k]<-median(load_cor[asia],na.rm=TRUE)
  regiondf$asia_f_diffuse[k]<-sum(Diffuse[asia])/(sum(Diffuse[asia])+sum(Point[asia]))
  
  regiondf$europe_conc_mean[k]<-mean(conc[europe],na.rm=TRUE)
  regiondf$europe_conc_median[k]<-median(conc[europe],na.rm=TRUE)
  regiondf$europe_load_mean[k]<-mean(load_cor[europe],na.rm=TRUE)
  regiondf$europe_load_median[k]<-median(load_cor[europe],na.rm=TRUE)
  regiondf$europe_f_diffuse[k]<-sum(Diffuse[europe])/(sum(Diffuse[europe])+sum(Point[europe]))
  
  regiondf$latinamerica_conc_mean[k]<-mean(conc[latinamerica],na.rm=TRUE)
  regiondf$latinamerica_conc_median[k]<-median(conc[latinamerica],na.rm=TRUE)
  regiondf$latinamerica_load_mean[k]<-mean(load_cor[latinamerica],na.rm=TRUE)
  regiondf$latinamerica_load_median[k]<-median(load_cor[latinamerica],na.rm=TRUE)
  regiondf$latinamerica_f_diffuse[k]<-sum(Diffuse[latinamerica])/(sum(Diffuse[latinamerica])+sum(Point[latinamerica]))
  
  regiondf$mena_conc_mean[k]<-mean(conc[mena],na.rm=TRUE)
  regiondf$mena_conc_median[k]<-median(conc[mena],na.rm=TRUE)
  regiondf$mena_load_mean[k]<-mean(load_cor[mena],na.rm=TRUE)
  regiondf$mena_load_median[k]<-median(load_cor[mena],na.rm=TRUE)
  regiondf$mena_f_diffuse[k]<-sum(Diffuse[mena])/(sum(Diffuse[mena])+sum(Point[mena]))
  
  regiondf$northamerica_conc_mean[k]<-mean(conc[northamerica],na.rm=TRUE)
  regiondf$northamerica_conc_median[k]<-median(conc[northamerica],na.rm=TRUE)
  regiondf$northamerica_load_mean[k]<-mean(load_cor[northamerica],na.rm=TRUE)
  regiondf$northamerica_load_median[k]<-median(load_cor[northamerica],na.rm=TRUE)
  regiondf$northamerica_f_diffuse[k]<-sum(Diffuse[northamerica])/(sum(Diffuse[northamerica])+sum(Point[northamerica]))
  
  regiondf$oceania_conc_mean[k]<-mean(conc[oceania],na.rm=TRUE)
  regiondf$oceania_conc_median[k]<-median(conc[oceania],na.rm=TRUE)
  regiondf$oceania_load_mean[k]<-mean(load_cor[oceania],na.rm=TRUE)
  regiondf$oceania_load_median[k]<-median(load_cor[oceania],na.rm=TRUE)
  regiondf$oceania_f_diffuse[k]<-sum(Diffuse[oceania])/(sum(Diffuse[oceania])+sum(Point[oceania]))
  print(k)
  
  ### plots
  #first define extent of the different continents, and crop the results to these extents
  e_africa <- extent(-20,60,-40,30)
  conc_africa <- crop(conc*africa, e_africa)
  load_africa <- crop(load*africa,e_africa)
  who_africa <- crop(who*africa,e_africa)
  who_africa[who_africa==0]<-NA
  
  e_asia <- extent(25,180,-12,80)
  conc_asia <- crop(conc*asia, e_asia)
  load_asia <- crop(load*asia,e_asia)
  who_asia <- crop(who*asia,e_asia)
  who_asia[who_asia==0]<-NA
  
  e_europe <- extent(-25,45,33,72)
  conc_europe <- crop(conc*europe, e_europe)
  load_europe <- crop(load*europe,e_europe)
  who_europe <- crop(who*europe,e_europe)
  who_europe[who_europe==0]<-NA
  
  e_latinamerica <- extent(-120,-30,-60,35)
  conc_latinamerica <- crop(conc*latinamerica, e_latinamerica)
  load_latinamerica <- crop(load*latinamerica,e_latinamerica)
  who_latinamerica <- crop(who*latinamerica,e_latinamerica)
  who_latinamerica[who_latinamerica==0]<-NA
  
  e_mena <- extent(-20,70,10,45)
  conc_mena <- crop(conc*mena, e_mena)
  load_mena <- crop(load*mena,e_mena)
  who_mena <- crop(who*mena,e_mena)
  who_mena[who_mena==0]<-NA
  
  e_northamerica <- extent(-180,-40,20,80)
  conc_northamerica <- crop(conc*northamerica, e_northamerica)
  load_northamerica <- crop(load*northamerica,e_northamerica)
  who_northamerica <- crop(who*northamerica,e_northamerica)
  who_northamerica[who_northamerica==0]<-NA
  
  e_oceania <- extent( 110, 180,-50,-5)
  conc_oceania <- crop(conc*oceania, e_oceania)
  load_oceania <- crop(load*oceania,e_oceania)
  who_oceania <- crop(who*oceania,e_oceania)
  who_oceania[who_oceania==0]<-NA
  
  #now create tiffs of the concentration maps

  tiff(paste0("conc_africa_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm", res=200) 
  plot(log10(conc_africa/1000), main=paste0("Oocyst conc. Africa ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()

  tiff(paste0("conc_asia_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(conc_asia/1000), main=paste0("Oocyst conc. Asia ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_europe_",mons[k],"1.tiff"),pointsize=15,width=17, height=15, units = "cm", res=200) 
  plot(log10(conc_europe/1000), main=paste0("Oocyst conc. Europe ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_latinamerica_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm", res=200) 
  plot(log10(conc_latinamerica/1000), main=paste0("Oocyst conc. Latin America ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_mena_",mons[k],"1.tiff"),pointsize=15,width=20, height=12, units = "cm", res=200) 
  plot(log10(conc_mena/1000), main=paste0("Oocyst conc. MENA ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_northamerica_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(conc_northamerica/1000), main=paste0("Oocyst conc. North America ",mons[k]," (oocysts/L)"),
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_oceania_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(conc_oceania/1000), main=paste0("Oocyst conc. Oceania ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors10,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
    plot(wrld_simpl, add = TRUE)
  dev.off()
  
  
  #now create tiffs of the concentration maps MASKED (for large rivers)
  #you can ignore the error messages about different extents, R solves this correctly
  dis_mask<-raster(paste0("D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/discharge_mask/dismask_200m3s.asc"))
  
  tiff(paste0("conc_africa_masked_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm", res=200) 
  plot(log10(conc_africa/1000*dis_mask), main=paste0("Oocyst conc. Africa ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_asia_masked_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(conc_asia/1000*dis_mask), main=paste0("Oocyst conc. Asia ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_europe_masked_",mons[k],"1.tiff"),pointsize=15,width=17, height=15, units = "cm", res=200) 
  plot(log10(conc_europe/1000*dis_mask), main=paste0("Oocyst conc. Europe ",mons[k]," (oocysts/L)"),
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_latinamerica_masked_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm", res=200) 
  plot(log10(conc_latinamerica/1000*dis_mask), main=paste0("Oocyst conc. Latin America ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_mena_masked_",mons[k],"1.tiff"),pointsize=15,width=20, height=12, units = "cm", res=200) 
  plot(log10(conc_mena/1000*dis_mask), main=paste0("Oocyst conc. MENA ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_northamerica_masked_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(conc_northamerica/1000*dis_mask), main=paste0("Oocyst conc. North America ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("conc_oceania_masked_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(conc_oceania/1000*dis_mask), main=paste0("Oocyst conc. Oceania ",mons[k]," (oocysts/L)"), 
       breaks=seq(-13,5,length.out = 100),col=colors11,
       xlab="Longitude (degrees)", ylab="Latitude (degrees)",
       axis.args=list(at=seq(-13,5,by=2),
                      labels=seq(-13,5,by=2), 
                      cex.axis=0.8),
       legend.args=list(text='Log10 
oocysts/L', side=3,line =0.5, cex=0.8))
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  
  #now create tiffs of the load maps
  
  tiff(paste0("load_africa_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm", res=200) 
  plot(log10(load_africa), main=paste0("Oocyst load Africa ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("load_asia_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(load_asia), main=paste0("Oocyst load Asia ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("load_europe_",mons[k],"1.tiff"),pointsize=15,width=17, height=15, units = "cm", res=200) 
  plot(log10(load_europe), main=paste0("Oocyst load Europe ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("load_latinamerica_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm", res=200) 
  plot(log10(load_latinamerica), main=paste0("Oocyst load Latin America ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("load_mena_",mons[k],"1.tiff"),pointsize=15,width=20, height=12, units = "cm", res=200) 
  plot(log10(load_mena), main=paste0("Oocyst load MENA ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("load_northamerica_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(load_northamerica), main=paste0("Oocyst load North America ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("load_oceania_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm", res=200) 
  plot(log10(load_oceania), main=paste0("Oocyst load Oceania ",mons[k]), col=colors2,xlab="Longitude (degrees)", ylab="Latitude (degrees)") #breaks=seq(-10,8,by=2),lab.breaks=brks,
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  #### now create WHO category maps
  
  tiff(paste0("who_africa_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm",  res=200)
  plot(who_africa, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("who_asia_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm",  res=200)
  plot(who_asia, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("who_europe_",mons[k],"1.tiff"),pointsize=15,width=17, height=15, units = "cm",  res=200)
  plot(who_europe, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("who_latinamerica_",mons[k],"1.tiff"),pointsize=15,width=15, height=15, units = "cm",  res=200)
  plot(who_latinamerica, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("who_mena_",mons[k],"1.tiff"),pointsize=15,width=20, height=12, units = "cm",  res=200)
  plot(who_mena, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  tiff(paste0("who_northamerica_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm",  res=200)
  plot(who_northamerica, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  
  #Oceania has several months in which category 6 is not reached, this messes up the color scale
  #fixed with an if else statement
  #months in which category 6 is reached: 1-5, 11, 12
  #months in which category 6 is not reached: 6-10
    
    if(k==6 || k==7 || k==8 || k==9 || k==10 )  {
  tiff(paste0("who_oceania_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm",  res=200)
  plot(who_oceania, col=colors3b, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  } else {
   tiff(paste0("who_oceania_",mons[k],"1.tiff"),pointsize=15,width=20, height=15, units = "cm",  res=200)
  plot(who_oceania, col=colors3, main=paste0("WHO pollution categories ",mons[k]), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  data(wrld_simpl)
  plot(wrld_simpl, add = TRUE)
  dev.off()
  }
  

  
}



filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/results_regional",Sys.Date(), ".csv")
write.csv(regiondf,file=filename) 




########## plot diffuse sources dominance log scale grid #################
#this script section creates the plot showing where point or diffuse sources dominate the result

Point<-raster("D:/LuciePhD/Model/Cryptomodel/modeloutput_humans/humanemissions.asc")/12 #divide by 12 for monthly
working.path.data<-"D:/LuciePhD/Model/Cryptomodel/modeldata/oocysts/"
Diffuse<- paste0(working.path.data,"diffuse_streaminput_1",mons,".asc")  
diffuse_stack<-stack(Diffuse)
diffuse_annual_mean<-mean(diffuse_stack,na.rm=TRUE)

pointhigher<-sum(Point,-1*(diffuse_annual_mean),na.rm=TRUE)
pointhigher[pointhigher<=0]<-NA
plot(log10(pointhigher))
diffusehigher<-sum(diffuse_annual_mean,-1*Point,na.rm=TRUE)
diffusehigher[diffusehigher<=0]<-NA
plot(log10(diffusehigher))
#check some stats
cellStats(diffusehigher,stat='min')
cellStats(diffusehigher,stat='max')
cellStats(pointhigher,stat='min')
cellStats(pointhigher,stat='max')

#make diffusehigher have negative numbers
pointhigher_log<-log10(pointhigher)
diffusehigher_log<-abs(log10(diffusehigher))*-1
cellStats(diffusehigher_log,stat='min')
cellStats(diffusehigher_log,stat='max')
cellStats(pointhigher_log,stat='min')
cellStats(pointhigher_log,stat='max')

#change one number that is -16.00234 to -16 for color scale of plot
diffusehigher_log[diffusehigher_log<= -16]<- -16

bb<-pointhigher_log>0
cc<-diffusehigher_log<0

combi<-raster(nrows=360,ncols=720)
combi[bb]<-pointhigher_log[bb]
combi[cc]<-diffusehigher_log[cc]
plot(combi)

out_dir <- "D:/LuciePhD/Model/Cryptomodel/modeloutput/"
setwd(out_dir)

#mask for regions in which discharge is below 1 using concentration mask
conc_annual <- raster("total_oocyst_concentration_annual_mean1.asc")
lowdischarge<-is.na(conc_annual)
combi[lowdischarge]<-NA

#define colors
colors_8<-c("blue4","blue1","deepskyblue","cadetblue2","grey85","burlywood1","orange1","red1","red4")

#load country border data 
data(countriesHigh)

#define breaks to add as label
brks1<-c(-16,-13,-10,-7,-3,3,7,10,13,15)
brks2<-c(expression(10^-16),expression(10^-13),expression(10^-10),expression(10^-7),expression(10^-3),expression(10^3),expression(10^7),expression(10^10),expression(10^13),expression(10^15))
brks3<-c(16,13,10,7,3,3,7,10,13,15)

#Single plot
tiff("Dominance_point_diffuse.tiff", width=50, height=30, units = "cm", pointsize=30, res=200) 
par(mar=c(5.5, 2.5, 2.5, 0.5) + 0.1)#par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(combi, col=colors_8,legend=FALSE, axes=FALSE, breaks=brks1)
plot(countriesHigh, add=TRUE)

plot(combi, legend.only=TRUE, col=colors_8,breaks=brks1, horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks3, 
                    cex.axis=0.8), #,font=1.4
     legend.args=list(text='Dominance of diffuse sources (blue) or point sources (red) log scale',cex=1)) #font=1.8,line=2.5, side=4, 
dev.off()

################ boxplots regional ######### 

out_dir <- "D:/LuciePhD/Model/Cryptomodel/modeloutput/"
setwd(out_dir)
  
  for(k in 1:12){
    
    conc <- raster(paste0("total_oocyst_concentration_",mons[k],"1.asc"))
    conc[conc==Inf]<-NA
    
    #create vectors with the concentration values of a certain continent in a certain month
    #divide by 1000 to go from m3 to liter
    assign(paste0("africa_vals_",mons[k]),conc[africa]/1000)
    assign(paste0("asia_vals_",mons[k]),conc[asia]/1000)
    assign(paste0("europe_vals_",mons[k]),conc[europe]/1000)
    assign(paste0("mena_vals_",mons[k]),conc[mena]/1000)
    assign(paste0("latinamerica_vals_",mons[k]),conc[latinamerica]/1000)
    assign(paste0("northamerica_vals_",mons[k]),conc[northamerica]/1000)
    assign(paste0("oceania_vals_",mons[k]),conc[oceania]/1000)
    
  }

#Combine vectors into dataframes
africa_vals<-data.frame(africa_vals_jan,africa_vals_feb,africa_vals_mar,africa_vals_apr,africa_vals_may,africa_vals_jun,africa_vals_jul,africa_vals_aug,africa_vals_sep,africa_vals_oct,africa_vals_nov,africa_vals_dec)
asia_vals<-data.frame(asia_vals_jan,asia_vals_feb,asia_vals_mar,asia_vals_apr,asia_vals_may,asia_vals_jun,asia_vals_jul,asia_vals_aug,asia_vals_sep,asia_vals_oct,asia_vals_nov,asia_vals_dec)
europe_vals<-data.frame(europe_vals_jan,europe_vals_feb,europe_vals_mar,europe_vals_apr,europe_vals_may,europe_vals_jun,europe_vals_jul,europe_vals_aug,europe_vals_sep,europe_vals_oct,europe_vals_nov,europe_vals_dec)
latinamerica_vals<-data.frame(latinamerica_vals_jan,latinamerica_vals_feb,latinamerica_vals_mar,latinamerica_vals_apr,latinamerica_vals_may,latinamerica_vals_jun,latinamerica_vals_jul,latinamerica_vals_aug,latinamerica_vals_sep,latinamerica_vals_oct,latinamerica_vals_nov,latinamerica_vals_dec)
mena_vals<-data.frame(mena_vals_jan,mena_vals_feb,mena_vals_mar,mena_vals_apr,mena_vals_may,mena_vals_jun,mena_vals_jul,mena_vals_aug,mena_vals_sep,mena_vals_oct,mena_vals_nov,mena_vals_dec)
northamerica_vals<-data.frame(northamerica_vals_jan,northamerica_vals_feb,northamerica_vals_mar,northamerica_vals_apr,northamerica_vals_may,northamerica_vals_jun,northamerica_vals_jul,northamerica_vals_aug,northamerica_vals_sep,northamerica_vals_oct,northamerica_vals_nov,northamerica_vals_dec)
oceania_vals<-data.frame(oceania_vals_jan,oceania_vals_feb,oceania_vals_mar,oceania_vals_apr,oceania_vals_may,oceania_vals_jun,oceania_vals_jul,oceania_vals_aug,oceania_vals_sep,oceania_vals_oct,oceania_vals_nov,oceania_vals_dec)

#do the same for annual mean oocyst concentrations
conc_rasters <- paste0("total_oocyst_concentration_",mons,i,".asc")
conc_stack<-stack(conc_rasters)
conc_stack[conc_stack==Inf]<-NA
conc_annual_mean<-mean(conc_stack,na.rm=TRUE)
assign("africa_vals_annual",conc_annual_mean[africa]/1000)
assign("asia_vals_annual",conc_annual_mean[asia]/1000)
assign("europe_vals_annual",conc_annual_mean[europe]/1000)
assign("latinamerica_vals_annual",conc_annual_mean[latinamerica]/1000)
assign("mena_vals_annual",conc_annual_mean[mena]/1000)
assign("northamerica_vals_annual",conc_annual_mean[northamerica]/1000)
assign("oceania_vals_annual",conc_annual_mean[oceania]/1000)
annual_vals<-list(africa_vals_annual,asia_vals_annual,europe_vals_annual,latinamerica_vals_annual,mena_vals_annual,northamerica_vals_annual,oceania_vals_annual)


#save dataframes to disk
write.csv(africa_vals,file = "africa_vals.csv")
write.csv(asia_vals,file = "asia_vals.csv")
write.csv(europe_vals,file = "europe_vals.csv")
write.csv(latinamerica_vals,file = "latinamerica_vals.csv")
write.csv(mena_vals,file = "mena_vals.csv")
write.csv(northamerica_vals,file = "northamerica_vals.csv")
write.csv(oceania_vals,file = "oceania_vals.csv")

continents<- c("Africa", "Asia", "Europe","Latin Am", "MENA", "North Am", "Oceania")
mn<-c(1:12)
ticks<-seq(1:7)
tick<-seq(1:12)

tiff("boxplots_regional_concentration.tiff",pointsize=12,width=25, height=15, units = "cm",  res=200)
par(mfrow=c(2,4), mar=c(3.5, 3, 3, 0.5) + 0.1,mgp=c(2, 1, 0))
boxplot(log10(africa_vals_annual), log10(asia_vals_annual),log10(europe_vals_annual),log10(latinamerica_vals_annual),log10(mena_vals_annual),log10(northamerica_vals_annual),log10(oceania_vals_annual),
      main="Regional annual average",
      xaxt='n', ylab="Concentration (log10 oocysts/L)")
      
      axis(side=1,at=ticks,labels=continents,las=2) 
     # text(x=ticks,srt=45,labels=continents)

boxplot(log10(africa_vals), main= "Africa",xaxt='n')
axis(side=1,at=tick,labels=mons,las=2) 
boxplot(log10(asia_vals),main="Asia",xaxt='n')
axis(side=1,at=tick,labels=mons,las=2) 
boxplot(log10(europe_vals),main="Europe",xaxt='n')
axis(side=1,at=tick,labels=mons,las=2) 
boxplot(log10(latinamerica_vals), main="Latin America",xaxt='n',ylab="Concentration (log10 oocysts/L)")
axis(side=1,at=tick,labels=mons,las=2) 
boxplot(log10(mena_vals), main= "MENA",xaxt='n')
axis(side=1,at=tick,labels=mons,las=2) 
boxplot(log10(northamerica_vals), main="North America",xaxt='n')
axis(side=1,at=tick,labels=mons,las=2) 
boxplot(log10(oceania_vals), main= "Oceania",xaxt='n')
axis(side=1,at=tick,labels=mons,las=2) 
dev.off()





########### decay plots ################

setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/survival")
colorsK<-colorRampPalette(c("dodgerblue", "green3", "gold","gold","gold", "darkorange","darkorange","darkorange","red1","red1", "red1", "purple", "purple", "purple"))(99)
colorsK<-colorRampPalette(rev(c("dodgerblue", "green3", "gold","gold","gold", "darkorange","darkorange","darkorange","red1","red1", "red1", "purple", "purple", "purple")))(99)
colorsK<-colorRampPalette(rev(c("blue", "dodgerblue", "green3", "gold", "darkorange","darkorange", "red1", "purple")))(99)

K_jan<-raster("K/K_jan.asc")
K_jul<-raster("K/K_jul.asc")
KL_jan<-raster("Lsurvival/kL_jan.asc")
KL_jul<-raster("Lsurvival/kL_jul.asc")
KT_jan<-raster("Tsurvival/kT_jan.asc")
KT_jul<-raster("Tsurvival/kT_jul.asc")
KS_jan<-raster("sedimentation/kS_jan.asc")
KS_jul<-raster("sedimentation/kS_jul.asc")
plot(K_jan,col=colorsK)
plot(K_jul,col=colorsK)
plot(KL_jan,col=colorsK)
plot(KL_jul,col=colorsK)
plot(KS_jan,col=colorsK)
plot(KS_jul,col=colorsK)
plot(KT_jan,col=colorsK)
plot(KT_jul,col=colorsK)

FS_K_jan<-exp(-1*K_jan)
plot(log10(FS_K_jan),col=colorsK)
FS_KL_jan<-exp(-1*KL_jan)
plot(log10(FS_KL_jan),col=colorsK)
FS_KS_jan<-exp(-1*KS_jan)
plot(log10(FS_KS_jan),col=colorsK)
FS_KT_jan<-exp(-1*KT_jan)
plot(log10(FS_KT_jan),col=colorsK)
FS_K_jul<-exp(-1*K_jul)
plot(log10(FS_K_jul),col=colorsK)
FS_KL_jul<-exp(-1*KL_jul)
plot(log10(FS_KL_jul),col=colorsK)
FS_KS_jul<-exp(-1*KS_jul)
plot(log10(FS_KS_jul),col=colorsK)
FS_KT_jul<-exp(-1*KT_jul)
plot(log10(FS_KT_jul),col=colorsK)

setwd("D:/LuciePhD/Model/Cryptomodel/modeloutput")

tiff("decay_plots.tiff",pointsize=12,width=17, height=19, units = "cm",  res=200)
par(mfrow=c(4,2), mar=c(1, 1, 3, 1) + 0.1,mgp=c(2, 1, 0))

plot(log10(FS_K_jan),col=colorsK,xaxt='n',yaxt='n', 
     main= "Overall loss 
     January (log10/day)")
plot(log10(FS_K_jul),col=colorsK, xaxt='n',yaxt='n',
     main= "Overall loss 
     July (log10/day)")
plot(log10(FS_KL_jan),col=colorsK, xaxt='n',yaxt='n',
     main="Radiation-dependent decay 
     January (log10/day)")
plot(log10(FS_KL_jul),col=colorsK,xaxt='n',yaxt='n',
     main="Radiation-dependent decay 
     July (log10/day)")
plot(log10(FS_KT_jan),col=colorsK,xaxt='n', yaxt='n',
     main="Temperature-dependent decay 
     January (log10/day)")
plot(log10(FS_KT_jul),col=colorsK, xaxt='n',yaxt='n',
     main="Temperature-dependent decay 
     July (log10/day)")
plot(log10(FS_KS_jan),col=colorsK,xaxt='n',yaxt='n',
     main="Sedimentation 
     January (log10/day)")
plot(log10(FS_KS_jul),col=colorsK,xaxt='n',yaxt='n', 
     main="Sedimentation 
     July (log10/day)")
dev.off()


################## high end low end run difference plot ###############
# this is run 38 (high) and run 39 (low)
average<-raster("D:/LuciePhD/Model/Cryptomodel/modeloutput/total_oocyst_concentration_annual_mean1.asc")
high<-raster("D:/LuciePhD/Model/Cryptomodel/modeloutput/total_oocyst_concentration_annual_mean38.asc")
low<-raster("D:/LuciePhD/Model/Cryptomodel/modeloutput/total_oocyst_concentration_annual_mean39.asc")

colorshighlow<-colorRampPalette(c("blue", "dodgerblue","green", "yellow", "orange","red",  "red4"))(99)

#mask for regions in which discharge is below 1 using concentration mask
conc_annual <- raster("D:/LuciePhD/Model/Cryptomodel/modeloutput/total_oocyst_concentration_annual_mean1.asc")
lowdischarge<-is.na(conc_annual)
average[lowdischarge]<-NA
low[lowdischarge]<-NA
high[lowdischarge]<-NA

#fix some weird extremely low values in the low grid
low[low<=1e-10]<-1e-10

#subtract
highlow<-high-low
highlow2<-high/low
highaverage<-high-average
averagelow<-average-low

plot(log10(average), main="average")
plot(log10(low),main="low")
plot(log10(high),main="high")
plot(log10(highlow),main="highlow",col=colorshighlow)
plot(log10(highlow2),main="highlow2",col=colorshighlow)
plot(highlow2,main="highlow2_notlog",col=colorshighlow)
plot(log10(highlow),main="highaverage",col=colorshighlow)
plot(log10(highlow),main="averagelow",col=colorshighlow)


meanhigh<-cellStats(high,stat='mean')
meanlow<-cellStats(low,stat='mean')
meanaverage<-cellStats(average,stat='mean')

