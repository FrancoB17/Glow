#New script for human emissions. Developed by Nynke Hofstra, May 2019

###Read the required data (subarea data, possibly WWTP data, and populationdata)

#open .csv file with sub-area specific data
HumanData<-read.csv(paste0(working.path.in,overall_inputs$HumanData_filename[i],".csv"),sep=",",header=TRUE,col.names=c("countryname","iso","hdi","pop_totalx1000","f_urb","f_rur","f_under5","f_5to14","f_15to24","f_25plus","con_urb", "con_rur","dir_urb","dir_rur","dif_urb","dif_rur","onsite_urb","onsite_rur","onsite_urb_treated","onsite_rur_treated","onsite_urb_water","onsite_rur_water","onsite_urb_land","onsite_rur_land","onsite_urb_flush","onsite_rur_flush","onsite_urb_remain","onsite_rur_remain", "treatm_1","treatm_2","treatm_3","treatm_4","treatm_ponds","treatm_no"),numerals="warn.loss")
HumanData<-HumanData[order(HumanData$iso),]
HumanData$pop_totalx1000<-HumanData$pop_totalx1000*1000

#we need to check whether or not the isorasters and the humandata have the same isocodes. I don't do that here as I
#am not sure how I could implement this yet. In the K2P app, the codes are generated from the isomap directly,
#so there is no need to do this now.

#add new column for the removalfraction
HumanData$remainingfraction<- 1-(HumanData$treatm_1*pathogen_inputs$RemovalPrimary[pathogen_row]+HumanData$treatm_2*pathogen_inputs$RemovalSecondary[pathogen_row]+HumanData$treatm_3*pathogen_inputs$RemovalTertiary[pathogen_row]+HumanData$treatm_4*pathogen_inputs$RemovalQuaternary[pathogen_row]+HumanData$treatm_ponds*pathogen_inputs$RemovalPonds[pathogen_row])

#find developed and developing countries
#make a grid with low HDI and high HDI
HDIboundary<-overall_inputs$HDIboundary[i]
hditemp<-data.frame(iso=HumanData$iso,value=HumanData$hdi)
HDI_raster<-subs(isoraster, hditemp, subsWithNA=T)
lowhdiraster<-isoraster
highhdiraster<-isoraster
lowhdiraster[]<-0
highhdiraster[]<-0
lowhdiraster[HDI_raster<HDIboundary]<-1
highhdiraster[HDI_raster>=HDIboundary]<-1
rm(hditemp,HDI_raster)

#open .csv file with WWTP data
if(WWTPData_available==TRUE){
  WWTP_inputs<-read.csv(paste0(working.path.in,overall_inputs$WWTPData_filename[i],".csv"))  #file has lat, lon, capacity, treatment type (if available) and reduction (if available) -> these data should come from Matt's model later on.
}

#read in population data
popurban_grid1<-raster(paste0(working.path.in,overall_inputs$population_urban_filename,".asc"))
poprural_grid1<-raster(paste0(working.path.in,overall_inputs$population_rural_filename,".asc"))

#crop the population data to the extent of the isoraster
popurban_grid1<-crop(popurban_grid1,area_extent)
poprural_grid1<-crop(poprural_grid1,area_extent)

#Now the gridded population data need to be corrected to match the HumanData populationdata when grids are summed
#I think it should be possible to do this without this lengthy loop, but not sure how.
popurb<-array(data=NA,dim=length(HumanData$iso))
poprur<-array(data=NA,dim=length(HumanData$iso))

for (j in 1:length(HumanData$iso)){
  popurb[j]<-sum(popurban_grid1[isoraster[]==HumanData$iso[j]],na.rm=TRUE)
  poprur[j]<-sum(poprural_grid1[isoraster[]==HumanData$iso[j]],na.rm=TRUE)
  print(j)
}
rm(j)

urbanpop2010<-HumanData$f_urb*HumanData$pop_total
ruralpop2010<-(1-HumanData$f_urb)*HumanData$pop_total
dif_urban<-urbanpop2010/popurb
dif_rural<-ruralpop2010/poprur

dif_urban<-data.frame(iso=HumanData$iso,value=dif_urban)
dif_urban_raster<-subs(isoraster, dif_urban, subsWithNA=T)

dif_rural<-data.frame(iso=HumanData$iso,value=dif_rural)
dif_rural_raster<-subs(isoraster, dif_rural, subsWithNA=T)

popurban_grid<-popurban_grid1*dif_urban_raster
poprural_grid<-poprural_grid1*dif_rural_raster

rm(popurban_grid1,poprural_grid1)

#end of gridded population correction


##put population in grids in categories

pop_fbelow5temp<-data.frame(iso=HumanData$iso,value=HumanData$f_under5)
pop_fbelow5<-subs(isoraster, pop_fbelow5temp, subsWithNA=T) #grids with <5 fraction
pop_fover5temp<-data.frame(iso=HumanData$iso,value=HumanData$f_5to14+HumanData$f_15to24+HumanData$f_25plus)
pop_fover5<-subs(isoraster, pop_fbelow5temp, subsWithNA=T) #grids with 5+ fraction

pop_furbcontemp<-data.frame(iso=HumanData$iso,value=HumanData$con_urb)
pop_furbcon<-subs(isoraster, pop_furbcontemp, subsWithNA=T) #grids with urban connected
pop_frurcontemp<-data.frame(iso=HumanData$iso,value=HumanData$con_urb)
pop_frurcon<-subs(isoraster, pop_frurcontemp, subsWithNA=T) #grids with rural connected
pop_furbdirtemp<-data.frame(iso=HumanData$iso,value=HumanData$dir_rur)
pop_furbdir<-subs(isoraster, pop_furbdirtemp, subsWithNA=T) #grids with urban direct
pop_frurdirtemp<-data.frame(iso=HumanData$iso,value=HumanData$dir_rur)
pop_frurdir<-subs(isoraster, pop_frurdirtemp, subsWithNA=T) #grids with rural direct
pop_furbdiftemp<-data.frame(iso=HumanData$iso,value=HumanData$dif_urb)
pop_furbdif<-subs(isoraster, pop_furbdiftemp, subsWithNA=T) #grids with urban diffuse
pop_frurdiftemp<-data.frame(iso=HumanData$iso,value=HumanData$dif_rur)
pop_frurdif<-subs(isoraster, pop_frurdiftemp, subsWithNA=T) #grids with rural diffuse
pop_furbonsitetreatedtemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_urb_treated)
pop_furbonsitetreated<-subs(isoraster, pop_furbonsitetreatedtemp, subsWithNA=T) #grids with urban onsite to treatment
pop_fruronsitetreatedtemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_rur_treated)
pop_fruronsitetreated<-subs(isoraster, pop_fruronsitetreatedtemp, subsWithNA=T) #grids with rural onsite to treatment
pop_furbonsitewatertemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_urb_water)
pop_furbonsitewater<-subs(isoraster, pop_furbonsitewatertemp, subsWithNA=T) #grids with urban onsite to water
pop_fruronsitewatertemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_rur_water)
pop_fruronsitewater<-subs(isoraster, pop_fruronsitewatertemp, subsWithNA=T) #grids with rural onsite to water
pop_furbonsitelandtemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_urb_land)
pop_furbonsiteland<-subs(isoraster, pop_furbonsitelandtemp, subsWithNA=T) #grids with urban onsite to land
pop_fruronsitelandtemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_rur_land)
pop_fruronsiteland<-subs(isoraster, pop_fruronsitelandtemp, subsWithNA=T) #grids with rural onsite to land
pop_furbonsiteflushtemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_urb_flush)
pop_furbonsiteflush<-subs(isoraster, pop_furbonsiteflushtemp, subsWithNA=T) #grids with urban onsite flushed
pop_fruronsiteflushtemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_rur_flush)
pop_fruronsiteflush<-subs(isoraster, pop_fruronsiteflushtemp, subsWithNA=T) #grids with rural onsite flushed
pop_furbonsiteremaintemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_urb_remain)
pop_furbonsiteremain<-subs(isoraster, pop_furbonsiteremaintemp, subsWithNA=T) #grids with urban onsite remained
pop_fruronsiteremaintemp<-data.frame(iso=HumanData$iso,value=HumanData$onsite_rur_remain)
pop_fruronsiteremain<-subs(isoraster, pop_fruronsiteremaintemp, subsWithNA=T) #grids with rural onsite remained

pop_urb_con<-popurban_grid*popfurbcon#population connected to sewer

#population with direct emissions (open defecation in urban areas and hanging toilets)

#population with diffuse emissions (open defecation in rural areas)

#population with onsite system emptied and taken to treatment

#population with onsite system emptied and dumped on land

#population with onsite system emptied and dumped in water

#population with onsite system not emptied, but flushed

#population with onsite system contained





