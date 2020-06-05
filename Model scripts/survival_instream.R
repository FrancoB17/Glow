# This script computes survival of oocysts in stream
# written by Lucie, version April 2017
# adjusted by Nynke in April 2019


if(pathogen=="cryptosporidium"){
  
  ########## temperature part #############
  #read in watertemperature data
  setwd(paste0(working.path.in,"watertemperature"))
  Tw_files<- list.files(path=(paste0(working.path.in,"watertemperature"))) 
  Tw<-stack(Tw_files)
  Tw<-crop(Tw,extent(isoraster_hydro))
  #plot(Tw,col=colors2)
  Tw<-Tw*overall_inputs$water_T[i]
  
  #we assume that survival in waters below 4 degrees Celsius is the same as at 4 degrees
  Tw[Tw < pathogen_inputs$TatKt[pathogen_row]] <- pathogen_inputs$TatKt[pathogen_row]
  
  Kt<-pathogen_inputs$Kt[pathogen_row]
  
  #define KT function
  #per day!
  k_T <- function(Tw)
  {
    Kt*exp(pathogen_inputs$lambda[pathogen_row]*(Tw-pathogen_inputs$TatKt[pathogen_row]))
  }
  
  #curve(k_T,0, 35)
  
  #apply function to Tw rasters
  KT<-calc(Tw,fun=k_T)
  

  ########## light part ######################
  
  #read in WATCH shortwave radiation data
  # Unit: W/m2. Watt equals Joule per second (average light intenstity)
  rad_dir <-paste0(working.path.in,"SWradiation") 
  rad_files<-list.files(path=rad_dir)
  setwd(rad_dir)
  rad<-stack(rad_files)
  rad<-crop(rad,extent(isoraster_hydro))
  #convert to kJ/m2 (per day)
  I<-rad*overall_inputs$radiation[i]*(60*60*24)/1000
  
  # read in DOC concentration here, made from Global NEWS values
  CDOC<- raster(paste0(working.path.in,"DOCraster.asc"))
  CDOC<-CDOC*overall_inputs$DOC[i]
  CDOC_multiplier<-xres(CDOC)/xres(isoraster_hydro)
  CDOC<-disaggregate(CDOC,fact=CDOC_multiplier)
  CDOC<-crop(CDOC,isoraster_hydro)
  
  #read in river depth files (in m)
  setwd(paste0(working.path.in,"rivergeometry/"))
  depth_files<-paste0("river_depth_",i,mons,".asc")
  Z<-stack(depth_files)
  Z<-Z*overall_inputs$depth[i]
  
  #define constants
  kl<-pathogen_inputs$kl[pathogen_row]
  kd<-pathogen_inputs$kd[pathogen_row]
  
  #define KL function
  k_L <- function(I, CDOC, Z) {
    (kl*I/(kd*CDOC*Z))*(1-exp(-kd*CDOC*Z))
  }
  
  KL<-overlay(I,CDOC,Z,fun=k_L)
  #plot(KL)
  

  ######## sedimentation part ######################
  
  #Ks = v / Z following Thomann and Mueller 1987 and Mancini 1978
  
  #river depth already read in in previous part
  
  #define kS function
  k_S <- function(Z) {
    v_settling/Z
  }
  
  v_settling<-pathogen_inputs$v_settling[pathogen_row] # standard 0.1 m/day, from Brookes et al. 2006
  KS<-calc(Z,fun=k_S)
  #plot(KS)
  
} else{
  print("ERROR: the model is currently unable to calculate concentrations for pathogens other than cryptosporidium")
  exit
}



############# Calculating survival during residence time ###########

#read in residence time (in months)
filenames3<-paste0(working.path.in,"rivergeometry/river_restime_",i,mons,".asc")
restime<-stack(filenames3)
restime<-restime*overall_inputs$restime[i]
#plot(restime,col=colors2)

#number of days per month 
mon_days<-365/12
restime_days<-restime*mon_days

K<-KT+KL+KS

# define survival function for survival during the residence time
surv_res<- function(K,t){
  exp(-K*t) 
}

#apply surv_res function and plot result
surv_total<-overlay(K,restime_days,fun=surv_res)
#plot(surv_total, col=colors2)
filenames4<-paste0("survival_restime_",i,mons,".asc")
setwd(paste0(working.path.in,"survival/survivalcombined"))
writeRaster(surv_total, filename=filenames4, format="ascii", bylayer=TRUE, overwrite=TRUE)

#only the temperature part
surv_temp<-overlay(KT,restime_days,fun=surv_res)
#plot(surv_temp)
filenames5<-paste0("Tsurvival_restime_",i,mons,".asc")
setwd(paste0(working.path.in,"survival/Tsurvival"))
writeRaster(surv_temp, filename=filenames5, format="ascii", bylayer=TRUE, overwrite=TRUE)

#only the light part
surv_light<-overlay(KL,restime_days,fun=surv_res)
#plot(surv_light)
filenames6<-paste0("Lsurvival_restime_",i,mons,".asc")
setwd(paste0(working.path.in,"survival/Lsurvival"))
writeRaster(surv_light, filename=filenames6, format="ascii", bylayer=TRUE, overwrite=TRUE)

#only the sedimentation part
surv_sedimentation<-overlay(KS,restime_days,fun=surv_res)
#plot(surv_sedimentation)
filenames8<-paste0("sedimentation_restime_",i,mons,".asc")
setwd(paste0(working.path.in,"survival/sedimentation"))
writeRaster(surv_sedimentation, filename=filenames8, format="ascii", bylayer=TRUE, overwrite=TRUE)

rm(k_L,k_S,k_T,surv_res,Tw,Tw_files,rad_dir,rad_files,rad)
rm(I,CDOC,depth_files,Z,filenames3,filenames4,filenames5,filenames6,filenames8)
rm(surv_light,surv_sedimentation,surv_temp,surv_total)
rm(restime,restime_days,K,KT,KL,KS,kd,kl,v_settling)

