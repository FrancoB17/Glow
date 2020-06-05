# the routing script, which takes all point/human and diffuse/animal emissions
# and routes these through the stream network, while accounting for in-stream survival

# updated by Nynke in April 2019

#Set working directory, load libraries, set working paths
setwd(wd)

working.path.data<-paste0(working.path.in,"oocysts/")
survival_dir <- paste0(working.path.in,"survival/")
river_dir <- working.path.in
VIC_dir <-  paste0(working.path.in,"discharge/")
out_dir <- working.path.out

conc_stack<-stack()
load_stack<-stack()

mons <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
months <- c("january","february","march","april","may","june","july","august","september","october","november","december")
mon_sec <- 2628000 #number of seconds in 365-day-year divided by 12

#### read in  flow direction and accumulation
#note: this script works with matrices in many places, that is a legacy of Marijkes work
#it is just another way of doing things, works fine here. 
dirmat<- as.matrix(raster(paste0(river_dir,overall_inputs$flowdirection_filename[i])))
flowmat<-as.matrix(raster(paste0(river_dir,overall_inputs$flowaccumulation_filename[i])))
dirmat<-rbind(0,dirmat,0)
dirmat<-cbind(0,dirmat,0)
flowmat<-rbind(0,flowmat,0)
flowmat<-cbind(0,flowmat,0)


#for faster running, define elements that are in flowmat
# sort and find unique elements in flowmat, exclude NA value, is of length 863
elements  <-  unique(as.vector(flowmat), incomparables = FALSE, MARGIN = 1,fromLast = FALSE)
elements  <- sort(elements[!is.na(elements)])
dir<-c(1,2,4,8,16,32,64,128) # the unique elements in flow direction

#Flow direction specification:
#+---+----+-----+
#|32 | 64 | 128 |
#+---+----+-----+
#|16 |    |  1  |
#+---+----+-----+
#|8  | 4  |  2  |
#+---+----+-----+

# Code 9 is outflow cell. These are found as a band at all coastlines, 
# and some in the middle of continents (endorheic basins)


################ loop ####

#### The input data are currently annual
#  Point<-matrix(0,360,720) #use this if you are only calculating for diffuse sources
Pointfile<-raster(paste0(working.path.in,"modeloutput_humans/humanemissions_",pathogen,i,".asc"))
Pointfile<-crop(Pointfile,extent(isoraster_hydro))
res_multiplier<-res(isoraster_hydro)/reso
Pointfile<-aggregate(Pointfile, fact=res_multiplier, fun=sum)
Point<-as.matrix(Pointfile)/12 #divide by 12 for monthly
Point[is.na(Point)] <- 0
Point<-Point*overall_inputs$human_loads[i]
Point<-rbind(0,Point,0)
Point<-cbind(0,Point,0)
#plot(raster(log10(Point),xmn=-180,xmx=180,ymn=-90,ymx=90),main="Point")

for(k in 1:12){ # loop over all months

  print(months[k]) 

  # Read input grids
  
#camels<- t(as.matrix(readAsciiGrid(paste(working.path.data,"diffuse_streaminput_camels_",mons[k],".asc",sep=""))))  
 # cattle<- t(as.matrix(readAsciiGrid(paste(working.path.data,"diffuse_streaminput_cattle_",mons[k],".asc",sep=""))))  
  #buffaloes<- t(as.matrix(readAsciiGrid(paste(working.path.data,"diffuse_streaminput_buffaloes_",mons[k],".asc",sep=""))))  
  #sheep<- t(as.matrix(readAsciiGrid(paste(working.path.data,"diffuse_streaminput_sheep_",mons[k],".asc",sep=""))))  
  #goats<- t(as.matrix(readAsciiGrid(paste(working.path.data,"diffuse_streaminput_goats_",mons[k],".asc",sep=""))))  
#Diffuse<-camels+cattle+buffaloes+sheep+goats
  
  Diffuse<- as.matrix(raster(paste0(working.path.data,"diffuse_streaminput_human_",pathogen,i,mons[k],".asc")))  
#  Diffuse<-matrix(0,360,720) #use this if you are only calculating for point sources
  Diffuse[is.na(Diffuse)] <- 0
  Diffuse<-rbind(0,Diffuse,0)
  Diffuse<-cbind(0,Diffuse,0)
#  plot(raster(log10(Diffuse),xmn=-180,xmx=180,ymn=-90,ymx=90),main="Diffuse")
  
  ### we want to know whether diffuse or point sources dominate
  #dominance<-Diffuse<Point
  #dominance_p<-Point<Diffuse
  #plot(raster(dominance,xmn=-180,xmx=180,ymn=-90,ymx=90),main=paste0("Diffuse sources dominate ",mons[k]))
  #plot(raster(dominance_p,xmn=-180,xmx=180,ymn=-90,ymx=90),main=paste0("Point sources dominate ",mons[k]))
  #add fraction of load that is from diffuse sources to results table
  fraction_diffuse<-sum(Diffuse)/(sum(Diffuse)+sum(Point))
  ifelse(k==1,results$f_diffuse_jan[i]<-fraction_diffuse,
         ifelse(k==2,results$f_diffuse_feb[i]<-fraction_diffuse,
                ifelse(k==3,results$f_diffuse_mar[i]<-fraction_diffuse, 
                       ifelse(k==4,results$f_diffuse_apr[i]<-fraction_diffuse, 
                              ifelse(k==5,results$f_diffuse_may[i]<-fraction_diffuse,
                                     ifelse(k==6,results$f_diffuse_jun[i]<-fraction_diffuse,
                                            ifelse(k==7,results$f_diffuse_jul[i]<-fraction_diffuse,
                                                   ifelse(k==8,results$f_diffuse_aug[i]<-fraction_diffuse,
                                                          ifelse(k==9,results$f_diffuse_sep[i]<-fraction_diffuse,
                                                                 ifelse(k==10,results$f_diffuse_oct[i]<-fraction_diffuse,
                                                                        ifelse(k==11,results$f_diffuse_nov[i]<-fraction_diffuse,
                                                                               results$f_diffuse_dec[i]<-fraction_diffuse)))))))))))
  
  ifelse(k==1,results$load_diffuse_jan[i]<-sum(Diffuse),
         ifelse(k==2,results$load_diffuse_feb[i]<-sum(Diffuse),
                ifelse(k==3,results$load_diffuse_mar[i]<-sum(Diffuse), 
                       ifelse(k==4,results$load_diffuse_apr[i]<-sum(Diffuse), 
                              ifelse(k==5,results$load_diffuse_may[i]<-sum(Diffuse),
                                     ifelse(k==6,results$load_diffuse_jun[i]<-sum(Diffuse),
                                            ifelse(k==7,results$load_diffuse_jul[i]<-sum(Diffuse),
                                                   ifelse(k==8,results$load_diffuse_aug[i]<-sum(Diffuse),
                                                          ifelse(k==9,results$load_diffuse_sep[i]<-sum(Diffuse),
                                                                 ifelse(k==10,results$load_diffuse_oct[i]<-sum(Diffuse),
                                                                        ifelse(k==11,results$load_diffuse_nov[i]<-sum(Diffuse),
                                                                               results$load_diffuse_dec[i]<-sum(Diffuse))))))))))))
  
  ifelse(k==1,results$load_point_jan[i]<-sum(Point),
         ifelse(k==2,results$load_point_feb[i]<-sum(Point),
                ifelse(k==3,results$load_point_mar[i]<-sum(Point), 
                       ifelse(k==4,results$load_point_apr[i]<-sum(Point), 
                              ifelse(k==5,results$load_point_may[i]<-sum(Point),
                                     ifelse(k==6,results$load_point_jun[i]<-sum(Point),
                                            ifelse(k==7,results$load_point_jul[i]<-sum(Point),
                                                   ifelse(k==8,results$load_point_aug[i]<-sum(Point),
                                                          ifelse(k==9,results$load_point_sep[i]<-sum(Point),
                                                                 ifelse(k==10,results$load_point_oct[i]<-sum(Point),
                                                                        ifelse(k==11,results$load_point_nov[i]<-sum(Point),
                                                                               results$load_point_dec[i]<-sum(Point))))))))))))
  
  
  # Discharge grid
  #data are monthly average discharge in m3/s
  dis<-as.matrix(raster(paste(VIC_dir,"discharge_",mons[k],"_",i,".asc", sep="")))
  dis[is.na(dis)] <- 0
  dis<-dis*overall_inputs$discharge[i]
  dis<-cbind(0,dis,0)
  dis<-rbind(0,dis,0)
  
  #grid cells with average discharge of less than 1 m3/s will be changed to zero discharge
  # this is done because otherwise the model calculates extremely high concentrations
  #for extremely dry and sparsely populated regions such as the Sahara
  #the model cannot be considered representative for such regions, so better not include them
  dis[dis < 1] <- 0
  #plot(raster(log10(dis),xmn=-180,xmx=180,ymn=-90,ymx=90),main="log10 dis (m3/s monthly average)")
 
  # Compute total monthly water volume in a grid cell from discharge (m3/s) 
  # and number of seconds in a month (s/month)
  # V in m3/month
  V<- dis*mon_sec
  #plot(raster(log10(V),xmn=-180,xmx=180,ymn=-90,ymx=90),main="log10 V (m3/month)")
 
  # Read survival grids 
    # SURVIVAL DURING RESIDENCE TIME
  surv_filename<-overall_inputs$surv_filename[i]
  surv<- as.matrix(raster(paste0(survival_dir,surv_filename,i,mons[k],".asc")))
  surv[is.na(surv)] <- 0
  surv<-rbind(0,surv,0)
  surv<-cbind(0,surv,0)
  #plot(raster(surv, xmn=-180,xmx=180,ymn=-90,ymx=90),main="Survival per residence time")

# initialize
  emptyGrid <- r <- newpollmat <- matrix(0,nrow=isoraster_hydro@nrows+2,ncol=isoraster_hydro@ncols+2)
  P_pollmat <- Point
  D_pollmat <- Diffuse

print("finished initialization")
  #Routing loop, this can take quite long!
  
  for (l in 1:length(elements)) 
    { 
      if (l==1) {print("starting routing loop")}
      m = elements[l]
      newlist<-list(r,r,r,r,r,r,r,r) 
      
      for(j in 1:length(dir)) 
        { 
          #tmp finds certain cells
          tmp<-which(flowmat==m & dirmat==dir[j], arr.ind=TRUE)   
          tmp2<- tmp
          
           if (dir[j]==dir[1])
            {
              tmp2[,2]<-tmp[,2]+1
              newlist[[1]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
            } # end if ->
          
          if (dir[j]==dir[2])
            {
              tmp2[,1]<-tmp[,1]+1
              tmp2[,2]<-tmp[,2]+1  
              newlist[[2]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
            } # end if ->v
          
          if (dir[j]==dir[3])
           {
              tmp2[,1]<-tmp[,1]+1  
              newlist[[3]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])  
           } # end if v 
         
          if (dir[j]==dir[4])
           {
              tmp2[,1]<-tmp[,1]+1 
              tmp2[,2]<-tmp[,2]-1
              newlist[[4]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
           }  # end if v<-
         
          if (dir[j]==dir[5])
          {
            tmp2[,2]<-tmp[,2]-1  
            newlist[[5]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
          } # end if <-
          
          if (dir[j]==dir[6])
          {
            tmp2[,1]<-tmp[,1]-1 
            tmp2[,2]<-tmp[,2]-1  
            newlist[[6]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
          } # end if ^<-
          
          if (dir[j]==dir[7])
          {
            tmp2[,1]<-tmp[,1]-1 
            newlist[[7]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
          } # end if ^
          
          if (dir[j]==dir[8])
          {
            tmp2[,1]<-tmp[,1]-1 
            tmp2[,2]<-tmp[,2]+1  
            newlist[[8]][tmp2]<-surv[tmp]*(D_pollmat[tmp]+P_pollmat[tmp]+newpollmat[tmp])
          } # end if ^->      
      }
    total<- Reduce('+',newlist)
    newpollmat<-newpollmat+total
    rm(newlist)
    rm(total)
  }

#newpollmat is now one grid too large, so is reduced in size here:
newpollmat<-newpollmat[2:(isoraster_hydro@nrows+1),2:(isoraster_hydro@ncols+1)]
P_pollmat<-P_pollmat[2:(isoraster_hydro@nrows+1),2:(isoraster_hydro@ncols+1)]
D_pollmat<-D_pollmat[2:(isoraster_hydro@nrows+1),2:(isoraster_hydro@ncols+1)]
V<-V[2:(isoraster_hydro@nrows+1),2:(isoraster_hydro@ncols+1)]

# newpollmat is only the part that has been routed
# this means that in all flow starting cells (flowmat = 0) newpollmat is also zero
# this is why newpollmat looks 'grainy' when plotted
# because newpollmat is only the routed part (e.g. from 'previous' grid cells),
# we have to add it to the load in the 'current' grid cell --> D_pollmat and P_pollmat

totalpollutionload<-newpollmat+D_pollmat+P_pollmat  

totalpollutionload[totalpollutionload==0] <- NA
V[V==0]<-NA
totalconcentration <- totalpollutionload/V

#compute fraction of routed part
fraction_notrouted<-(D_pollmat+P_pollmat)/totalpollutionload
Fnotrouted<-raster(fraction_notrouted,xmn=isoraster_hydro@extent@xmin,xmx=isoraster_hydro@extent@xmax,ymn=isoraster_hydro@extent@ymin,ymx=isoraster_hydro@extent@ymax)
#plot(Fnotrouted, main= "Fraction of total pollution load that was not routed")
fraction_routed<-newpollmat/totalpollutionload
Frouted<-raster(fraction_routed,xmn=isoraster_hydro@extent@xmin,xmx=isoraster_hydro@extent@xmax,ymn=isoraster_hydro@extent@ymin,ymx=isoraster_hydro@extent@ymax)
#plot(Frouted, main= "Fraction of total pollution load that was routed")


#flow<-raster(flowmat,xmn=-180,xmx=180,ymn=-90,ymx=90)
#flow_start<-flow == 0
#plot(flow_start)
#newpo<-raster(newpollmat,xmn=-180,xmx=180,ymn=-90,ymx=90)
#Dpo<-raster(D_pollmat,xmn=-180,xmx=180,ymn=-90,ymx=90)
#Ppo<-raster(P_pollmat,xmn=-180,xmx=180,ymn=-90,ymx=90) 
#test<-getValues(newpo[flow_start])
#plot(log10(newpo), main="newpollmat")
#plot(log10(Dpo), main="diffuse pollmat")
#plot(log10(Ppo), main="point pollmat")
# Some things to test if the routing is going well
#newNA<-newpo==0
#newNA<-newNA*2
#DNA<-Dpo==0
#PNA<-Ppo==0
#DNAPNA<-Dpo==0 & Ppo==0
#NAsum<-newNA+DNA+PNA
#plot(NAsum)
#NAsum2<-DNAPNA+newNA

#cellStats(newpo,stat=mean)
#cellStats(Dpo,stat=mean)
#cellStats(newpo,stat=sum)
#cellStats(Dpo,stat=sum)

b <- raster(totalpollutionload,xmn=isoraster_hydro@extent@xmin,xmx=isoraster_hydro@extent@xmax,ymn=isoraster_hydro@extent@ymin,ymx=isoraster_hydro@extent@ymax)
c <- raster(totalconcentration,xmn=isoraster_hydro@extent@xmin,xmx=isoraster_hydro@extent@xmax,ymn=isoraster_hydro@extent@ymin,ymx=isoraster_hydro@extent@ymax)
#plot(log10(b), main = paste0('load log ', mons[k]))
#plot(log10(c), main =paste0('conc log ', mons[k]))

setwd(out_dir)
writeRaster(raster(newpollmat,xmn=isoraster_hydro@extent@xmin,xmx=isoraster_hydro@extent@xmax,ymn=isoraster_hydro@extent@ymin,ymx=isoraster_hydro@extent@ymax),filename=paste0("routed_oocysts_",pathogen,mons[k],run,".asc"),format="ascii",overwrite=TRUE)
writeRaster(Frouted,filename=paste0("routed_fraction_",pathogen,mons[k],run,".asc"),format="ascii",overwrite=TRUE)
writeRaster(b,filename=paste0("total_oocyst_load_",pathogen,mons[k],run,".asc"),format="ascii",overwrite=TRUE)
writeRaster(c,filename=paste0("total_oocyst_concentration_",pathogen,mons[k],run,".asc"),format="ascii",overwrite=TRUE)
conc_stack<-stack(conc_stack,c)
load_stack<-stack(load_stack,b)

#clean memory
rm(surv,D_pollmat,Diffuse,P_pollmat,V,dis,tmp,tmp2,r,b,c,totalpollutionload,totalconcentration,newpollmat) 
rm(Fnotrouted,Frouted,fraction_notrouted,fraction_routed)
rm(j,l,m)

#closing bracket entire loop
}

totalload<-calc(load_stack,fun=sum,na.rm=TRUE)
avgconc<-calc(conc_stack,fun=mean,na.rm=TRUE)

writeRaster(totalload,filename=paste0(working.path.out,"total_oocyst_load_", pathogen,"sum",run,".asc"),format="ascii",overwrite=TRUE)
writeRaster(avgconc,filename=paste0(working.path.out,"total_oocyst_concentration_", pathogen,"avg",run,".asc"),format="ascii",overwrite=TRUE)

#Make .png plot

colors<-c("slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")

avgconc[avgconc==0]<-NA
logavgconc<-log10(avgconc)
mx<-max(logavgconc[],na.rm=TRUE)
mn<-min(logavgconc[],na.rm=TRUE)
brks<-c(floor(mn),floor(mn)+(ceiling(mx)-floor(mn))/10,floor(mn)+2*(ceiling(mx)-floor(mn))/10,floor(mn)+3*(ceiling(mx)-floor(mn))/10,floor(mn)+4*(ceiling(mx)-floor(mn))/10,floor(mn)+5*(ceiling(mx)-floor(mn))/10,floor(mn)+6*(ceiling(mx)-floor(mn))/10,floor(mn)+7*(ceiling(mx)-floor(mn))/10,floor(mn)+8*(ceiling(mx)-floor(mn))/10,floor(mn)+9*(ceiling(mx)-floor(mn))/10,ceiling(mx))

#plot Png file
filename_png<-paste0(working.path.out,"total_oocysts_concentrations_",pathogen,run,".png")
png(filename=filename_png,width=750,height=750,units="px")

par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n") 
plot(logavgconc, col=colors, breaks=brks,legend=FALSE, axes=FALSE)
if(isTRUE(scenario_run)){
  title(main=paste0("Annual average concentrations scenario"))  
}else{
  title(main=paste0("Annual average concentrations baseline run"))  
}

#if the resolution is 0.08333 or 0.008333, the district data or higher res need to be plotted, if the resolution is 0.5, the country data needs to be plotted
if(reso == 0.5){
#  plot(countriesHigh, add=TRUE)
}else{
  setwd(paste0(working.path.in,"districts_shapefile/"))
  plot(readOGR(dsn = ".", layer = overall_inputs$shapefile_filename[i]),add=TRUE)
  setwd<-wd
}

if(pathogen=="cryptosporidium"){  
  plot(logavgconc, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=brks,
                      labels=brks, 
                      cex.axis=1,font=1.4),
       legend.args=list(text='Cryptosporidium concentrations (log10 oocysts / l)', font=1.8, cex=1.2)) 
}else if(pathogen=="rotavirus"){
  plot(logavgconc, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=brks,
                      labels=brks, 
                      cex.axis=1,font=1.4),
       legend.args=list(text='Rotavirus concentrations (log10 viral particles / ;)', font=1.8, cex=1.2)) 
}
dev.off()

#end of plotting .png file

writeRaster(avgconc,paste0(working.path.out,"total_oocysts_concentration",pathogen,run),format="GTiff",overwrite=TRUE)

rm(Pointfile,CDOC_multiplier,filenames,filenames2,fraction_diffuse,h,jj,Kt,res_multiplier,res_runoff,river_dir,split2,survival_dir,VIC_dir)
rm(k)
rm(dirmat,emptyGrid,flowmat,Point)
rm(dir,elements,surv_filename)

