# human emissions model developed by Lucie and Nynke
# updated to run from GloWPa_model.R in May 2019 by Nynke
# option to include location of WWTP has been added
# emissions to land have been separated and onsite sytems have been included


# Read in the human data input file and the WWTP data file ----------------
# For the current version of the model (and I think we should keep that in addition to the
# merged version), the human data file has 4 sanitation categories. Matt's version of the 
# model has 15 sanitation categories, which means the .csv file for the merged tool is
# different from the .csv file for the current tool.

#open .csv file with sub-area specific data
#requirements for / consequences of the HumanData file:
#con+dif+dir+onsite should be 1 for urban and 1 for rural
#onsite_treated+onsite_water+onsite_land+onsite_flush+onsite_land should be 1 for urban and 1 for rural
#treatm_1+treatm2+treatm3+treatm_4+treatm_ponds+treatm_no should be 1

#In case wwtpdata_available==TRUE: only in the districts with WWTPs the treatment fractions are used; this fraction is then applied for all waste treated in that district.

HumanData<-read.csv(paste0(working.path.in,overall_inputs$HumanData_filename,".csv"),sep=",",header=TRUE,col.names=c("countryname","iso","hdi","pop_totalx1000","f_urb","f_rur","f_under5","f_over5","con_urb", "con_rur","dir_urb","dir_rur","dif_urb","dif_rur","onsite_urb","onsite_rur","onsite_urb_treated","onsite_rur_treated","onsite_urb_water","onsite_rur_water","onsite_urb_land","onsite_rur_land","onsite_urb_flush","onsite_rur_flush","onsite_urb_remain","onsite_rur_remain", "treatm_1","treatm_2","treatm_3","treatm_4","treatm_ponds","treatm_no","storage_empty_urb","storage_empty_rur","storage_flush_urb","storage_flush_rur"),numerals="warn.loss") #paste0(overall_inputs$HumanData_filename[i],".csv"
HumanData<-HumanData[order(HumanData$iso),]
HumanData$pop_totalx1000<-HumanData$pop_totalx1000*overall_inputs$population_multiplication[i]

#open .csv file with WWTP data
# if 3 then location of WWTP known
# if 2 then treatment, but location unknown
# if 1 then no treatment

if(wwtp_available==3){
  WWTP_inputs<-read.csv(paste0(working.path.in,overall_inputs$WWTPData_filename[i],".csv"))  #file has district, lat, lon, capacity, system categories. Would be good to add treatment type (if available) and reduction (if available) -> these data should come from Matt's model later on. For now just average removal has been used.
  WWTP_inputs$treatment_type<-as.character(WWTP_inputs$treatment_type)
  WWTP_inputs$subregion<-as.character(WWTP_inputs$subregion)
}

if(wwtp_available==1){
  HumanData$treatm_1[]<-0
  HumanData$treatm_2[]<-0
  HumanData$treatm_3[]<-0
  HumanData$treatm_4[]<-0
  HumanData$treatm_ponds[]<-0
  HumanData$treatm_no[]<-1
}


# End to reading in human and WWTP input data. ----------------------------


# Check for isocodes - does not currently work ----------------------------

#we need to check whether or not the isorasters and the humandata have the same isocodes
#this bit currently does not run, because the isogrid file may have fewer grids than the HumanData$iso list
#(e.g. small islands are removed), so I need to find a way to compare lists that are not necessarily of equal
#length and I don't know how to do this yet.

#if(isoreadformat==1){
#  temp<-areas$OBJECTID-HumanData$iso
#  temp1<-sum(temp,na.rm=TRUE)
#  if(temp1>0){
#    print("ERROR, isorasters do not comply with the humandata file")
#  }
#  rm(temp, temp1)
#} else if(isoreadformat==2){
#  ##Add here a check, by making a list of the isocodes in the isoraster (figure out how to do this) and doing the same check as above.
#  temp0<-unique(isoraster,na.last=NA)
#  temp<-temp0[2:length(temp0)]-HumanData$iso
#  temp1<-sum(temp,na.rm=TRUE)
#  if(temp1>0){
#    print("ERROR, isorasters do not comply with the humandata file")
#  }
#  rm(temp, temp1)
#}


# End to check for isocodes -----------------------------------------------


# Read in population data and data correction for population in csv file --------
# Currently we are using landscan. I am considering to change to popgrid, as this has more options.
# I would prefer to change to a datafile that we can also use for future scenarios. Not sure if that exists.

#read in gridded urban and rural population data

popurban_grid1<-raster(paste0(working.path.in,overall_inputs$population_urban_filename[i]))
poprural_grid1<-raster(paste0(working.path.in,overall_inputs$population_rural_filename[i]))

#check that the correct resolution population data have been read in
if((reso-xres(popurban_grid1))>1e-5 || (reso-yres(popurban_grid1))>1e5 || (reso-xres(poprural_grid1))>1e-5 || (reso-yres(poprural_grid1))>1e-5){
  print("ERROR: population file(s) are incorrect resolution!")
  quit
}

#crop the population data to the extent of the isoraster
popurban_grid1<-crop(popurban_grid1,area_extent)
poprural_grid1<-crop(poprural_grid1,area_extent)

#Now the gridded population data need to be corrected to match the HumanData populationdata when grids are summed
#first make arrays with the summed urban and rural populations over the grids in the iso codes. This bit was
#written with help of Benjamin Brede - thanks to him it sped up a lot, compared to the loop I used first.

library(dplyr)
all_data <- data.frame(Urban = values(popurban_grid1),Rural = values(poprural_grid1),ISO = values(isoraster))
summed <- all_data %>%
  filter(ISO %in% HumanData$iso) %>% # restrict to only ISOs in HumanData_iso
  group_by(ISO) %>% # do the following per ISO code
  summarise(UrbanTotal = sum(Urban, na.rm = TRUE),
            RuralTotal = sum(Rural, na.rm = TRUE)) %>%
  ungroup()
HumanData <- HumanData[(HumanData$iso %in% summed$ISO), ]

HumanData$popurb<-NA
HumanData$poprur<-NA

for (j in 1:length(HumanData$iso)){
  q<-which(summed$ISO==HumanData$iso[j])
  if(length(q)>0){
    HumanData$popurb[q]<-summed$UrbanTotal[q]
    HumanData$poprur[q]<-summed$RuralTotal[q]
  }
}

rm(all_data)
rm(summed)

detach("package:dplyr", unload=TRUE)
#.rs.unloadPackage("dplyr")

#then the population grids need to be corrected for the difference between the summed gridded population and
#the HumanData population (the latter is supposed to have the most correct values.)

urbanpop2010<-HumanData$f_urb*HumanData$pop_total
ruralpop2010<-(1-HumanData$f_urb)*HumanData$pop_total

dif_urban<-urbanpop2010/HumanData$popurb
dif_rural<-ruralpop2010/HumanData$poprur

dif_urban<-data.frame(iso=HumanData$iso,value=dif_urban)
dif_urban_raster<-subs(isoraster, dif_urban, subsWithNA=T)

dif_rural<-data.frame(iso=HumanData$iso,value=dif_rural)
dif_rural_raster<-subs(isoraster, dif_rural, subsWithNA=T)

popurban_grid<-popurban_grid1*dif_urban_raster
poprural_grid<-poprural_grid1*dif_rural_raster

rm(popurban_grid1,poprural_grid1)

#end of gridded population correction

# End of reading in population and correction. ----------------------------


# Calculation of emissions per sanitation type and subarea -----------------------
# Be aware: in case we have WWTP location data, the reduction in those has not yet been added
# to the emissions. That is why after this section the point dataframe cannot yet be saved as file.

#fill all the added columns
HumanData$removalfraction<-1-(HumanData$treatm_1*pathogen_inputs$RemovalPrimary[pathogen_row]+HumanData$treatm_2*pathogen_inputs$RemovalSecondary[pathogen_row]+HumanData$treatm_3*pathogen_inputs$RemovalTertiary[pathogen_row]+HumanData$treatm_4*pathogen_inputs$RemovalQuaternary[pathogen_row]+HumanData$treatm_ponds*pathogen_inputs$RemovalPonds[pathogen_row])
HumanData$pop_urb_num<-HumanData$pop_totalx1000*HumanData$f_urb
HumanData$pop_rur_num<-HumanData$pop_totalx1000*HumanData$f_rur
HumanData$pop_urb_under5<- HumanData$pop_urb_num*HumanData$f_under5
HumanData$pop_rur_under5<- HumanData$pop_rur_num*HumanData$f_under5
HumanData$pop_urb_over5<- HumanData$pop_urb_num*HumanData$f_over5
HumanData$pop_rur_over5<- HumanData$pop_rur_num*HumanData$f_over5

#make dataframe to fill with urban and rural pathogen emissions
point<-array(dim=c(length(HumanData$iso),34))
colnames(point)<-c("countryname","iso","pathogen_urb_con","pathogen_rur_con","pathogen_urb_dir","pathogen_rur_dir","pathogen_urb_dif","pathogen_rur_dif","pathogen_urb_onsite_treated","pathogen_rur_onsite_treated","pathogen_urb_onsite_water","pathogen_rur_onsite_water","pathogen_urb_onsite_land","pathogen_rur_onsite_land","pathogen_urb_onsite_flush","pathogen_rur_onsite_flush","pathogen_urb_WWTP_sewer","pathogen_rur_WWTP_sewer","pathogen_urb_WWTP_onsite","pathogen_rur_WWTP_onsite","pathogen_urb_conforgrid","pathogen_rur_conforgrid","pathogen_urb_waterforgrid","pathogen_rur_waterforgrid","pathogen_urb_landforgrid","pathogen_rur_landforgrid","pathogen_urb_waterforgrid_pp","pathogen_rur_waterforgrid_pp","pathogen_urb_landforgrid_pp","pathogen_rur_landforgrid_pp","pathogen_urb_fconsewer","pathogen_rur_fconsewer","pathogen_urb_flandonsite","pathogen_rur_flandonsite")
point<-as.data.frame(point)

#fill dataframe
point$countryname<-HumanData$countryname
point$iso<-HumanData$iso

#find developed and developing countries & identify excretion categories
#a = developing, b=developed
#identify countries that fall into the different excretion categories
HDIboundary<-overall_inputs$HDIboundary[i]
a<-which(HumanData$hdi <= HDIboundary) #low HDI
b<-which(HumanData$hdi > HDIboundary)  #high HDI

NappyCor<-overall_inputs$NappyCor[i]
UrbanDirect2Water<-overall_inputs$UrbanDirect2Water[i]
RuralDirect2Water<-overall_inputs$RuralDirect2Water[i]
UrbanDiffuse2Water<-overall_inputs$UrbanDiffuse2Water[i]
RuralDiffuse2Water<-overall_inputs$RuralDiffuse2Water[i]
UrbanOnsite2Water<-overall_inputs$UrbanOnsite2Water[i]
RuralOnsite2Water<-overall_inputs$RuralOnsite2Water[i]
Kpit<-pathogen_inputs$Kpit[pathogen_row]

#this function calculates the decay in the onsite systems
f2<-function(t){
  V<-exp(-Kpit*t*365)
  return(V)
}

HumanData$survival_pit_empty_urb<-NA
HumanData$survival_pit_empty_rur<-NA
HumanData$survival_pit_flush_urb<-NA
HumanData$survival_pit_flush_rur<-NA

#for HDI high
if(length(b)>0){
  for (j in 1:length(HumanData$iso[b])){ #calculate per subarea
    #calculate decay in onsite systems
    int<-integrate(f2,0,HumanData$storage_empty_urb[b[j]])
    HumanData$survival_pit_empty_urb[b[j]]<-int$value/HumanData$storage_empty_urb[b[j]]
    int<-integrate(f2,0,HumanData$storage_empty_rur[b[j]])
    HumanData$survival_pit_empty_rur[b[j]]<-int$value/HumanData$storage_empty_rur[b[j]]

    int1<-integrate(f2,0,HumanData$storage_flush_urb[b[j]])
    HumanData$survival_pit_flush_urb[b[j]]<-int1$value/HumanData$storage_flush_urb[b[j]]
    int1<-integrate(f2,0,HumanData$storage_flush_rur[b[j]])
    HumanData$survival_pit_flush_rur[b[j]]<-int1$value/HumanData$storage_flush_rur[b[j]]

    #calculate the emissions by the specific population groups
    #in case people are interested in children/adults, these number can be split into <5 and >5, but this complicates the model
    point$pathogen_urb_con[b[j]]<-HumanData$con_urb[b[j]]*(ExcretionHighHDI_child*NappyCor*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])
    point$pathogen_rur_con[b[j]]<-HumanData$con_rur[b[j]]*(ExcretionHighHDI_child*NappyCor*HumanData$pop_rur_under5[b[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[b[j]])
    point$pathogen_urb_dir[b[j]]<-HumanData$dir_urb[b[j]]*UrbanDirect2Water*(ExcretionHighHDI_child*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])
    point$pathogen_rur_dir[b[j]]<-HumanData$dir_rur[b[j]]*RuralDirect2Water*(ExcretionHighHDI_child*HumanData$pop_rur_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_rur_over5[b[j]])
    point$pathogen_urb_dif[b[j]]<-HumanData$dif_urb[b[j]]*UrbanDiffuse2Water*(ExcretionHighHDI_child*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])
    point$pathogen_rur_dif[b[j]]<-HumanData$dif_rur[b[j]]*RuralDiffuse2Water*(ExcretionHighHDI_child*HumanData$pop_rur_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_rur_over5[b[j]])
    point$pathogen_urb_onsite_treated[b[j]]<-HumanData$onsite_urb_treated[b[j]]*HumanData$survival_pit_empty_urb[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])/HumanData$storage_empty_urb[b[j]]
    point$pathogen_rur_onsite_treated[b[j]]<-HumanData$onsite_rur_treated[b[j]]*HumanData$survival_pit_empty_rur[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_rur_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_rur_over5[b[j]])/HumanData$storage_empty_rur[b[j]]
    point$pathogen_urb_onsite_water[b[j]]<-HumanData$onsite_urb_water[b[j]]*HumanData$survival_pit_empty_urb[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])/HumanData$storage_empty_urb[b[j]]
    point$pathogen_rur_onsite_water[b[j]]<-HumanData$onsite_rur_water[b[j]]*HumanData$survival_pit_empty_rur[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_rur_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_rur_over5[b[j]])/HumanData$storage_empty_rur[b[j]]
    point$pathogen_urb_onsite_land[b[j]]<-HumanData$onsite_urb_land[b[j]]*HumanData$survival_pit_empty_urb[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])/HumanData$storage_empty_urb[b[j]]
    point$pathogen_rur_onsite_land[b[j]]<-HumanData$onsite_rur_land[b[j]]*HumanData$survival_pit_empty_rur[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_rur_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_rur_over5[b[j]])/HumanData$storage_empty_rur[b[j]]
    point$pathogen_urb_onsite_flush[b[j]]<-HumanData$onsite_urb_flush[b[j]]*HumanData$survival_pit_flush_urb[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_urb_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_urb_over5[b[j]])/HumanData$storage_flush_urb[b[j]]
    point$pathogen_rur_onsite_flush[b[j]]<-HumanData$onsite_rur_flush[b[j]]*HumanData$survival_pit_flush_rur[b[j]]*UrbanOnsite2Water*(ExcretionHighHDI_child*NappyCor*HumanData$pop_rur_under5[b[j]]+ExcretionHighHDI_others*HumanData$pop_rur_over5[b[j]])/HumanData$storage_flush_rur[b[j]]

    if(wwtp_available==3){

      #In this case all emissions from the connected population are not yet included in the emissions to water. These are added to the grids in the next step. Treatment has not been applied yet.
      point$pathogen_urb_conforgrid[b[j]]<-point$pathogen_urb_con[b[j]]+point$pathogen_urb_onsite_treated[b[j]]
      point$pathogen_rur_conforgrid[b[j]]<-point$pathogen_rur_con[b[j]]+point$pathogen_rur_onsite_treated[b[j]]
      point$pathogen_urb_waterforgrid[b[j]]<-point$pathogen_urb_dir[b[j]]+point$pathogen_urb_onsite_water[b[j]]+point$pathogen_urb_onsite_flush[b[j]]
      point$pathogen_rur_waterforgrid[b[j]]<-point$pathogen_rur_dir[b[j]]+point$pathogen_rur_onsite_water[b[j]]+point$pathogen_rur_onsite_flush[b[j]]
      point$pathogen_urb_landforgrid[b[j]]<-point$pathogen_urb_dif[b[j]]+point$pathogen_urb_onsite_land[b[j]]
      point$pathogen_rur_landforgrid[b[j]]<-point$pathogen_rur_dif[b[j]]+point$pathogen_rur_onsite_land[b[j]]

    } else{

      #In this case all emissions from the connected population go to the water after treatment.
      point$pathogen_urb_conforgrid[b[j]]<-NA
      point$pathogen_rur_conforgrid[b[j]]<-NA
      point$pathogen_urb_waterforgrid[b[j]]<-point$pathogen_urb_con[b[j]]*HumanData$removalfraction[b[j]]+point$pathogen_urb_dir[b[j]]+point$pathogen_urb_onsite_treated[b[j]]*HumanData$removalfraction[b[j]]+point$pathogen_urb_onsite_water[b[j]]+point$pathogen_urb_onsite_flush[b[j]]
      point$pathogen_rur_waterforgrid[b[j]]<-point$pathogen_rur_con[b[j]]*HumanData$removalfraction[b[j]]+point$pathogen_rur_dir[b[j]]+point$pathogen_rur_onsite_treated[b[j]]*HumanData$removalfraction[b[j]]+point$pathogen_rur_onsite_water[b[j]]+point$pathogen_rur_onsite_flush[b[j]]
      point$pathogen_urb_landforgrid[b[j]]<-point$pathogen_urb_dif[b[j]]+point$pathogen_urb_onsite_land[b[j]]
      point$pathogen_rur_landforgrid[b[j]]<-point$pathogen_rur_dif[b[j]]+point$pathogen_rur_onsite_land[b[j]]

    }
  }
  rm(j)
}

#for HDI low

if(length(a)>0){
  for (j in 1:length(HumanData$iso[a])){
    int<-integrate(f2,0,HumanData$storage_empty_urb[a[j]])
    HumanData$survival_pit_empty_urb[a[j]]<-int$value/HumanData$storage_empty_urb[a[j]]
    int<-integrate(f2,0,HumanData$storage_empty_rur[a[j]])
    HumanData$survival_pit_empty_rur[a[j]]<-int$value/HumanData$storage_empty_rur[a[j]]

    int1<-integrate(f2,0,HumanData$storage_flush_urb[a[j]])
    HumanData$survival_pit_flush_urb[a[j]]<-int1$value/HumanData$storage_flush_urb[a[j]]
    int1<-integrate(f2,0,HumanData$storage_flush_rur[a[j]])
    HumanData$survival_pit_flush_rur[a[j]]<-int1$value/HumanData$storage_flush_rur[a[j]]

    point$pathogen_urb_con[a[j]]<-HumanData$con_urb[a[j]]*(ExcretionLowHDI_child*NappyCor*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])
    point$pathogen_rur_con[a[j]]<-HumanData$con_rur[a[j]]*(ExcretionLowHDI_child*NappyCor*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])
    point$pathogen_urb_dir[a[j]]<-HumanData$dir_urb[a[j]]*UrbanDirect2Water*(ExcretionLowHDI_child*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])
    point$pathogen_rur_dir[a[j]]<-HumanData$dir_rur[a[j]]*RuralDirect2Water*(ExcretionLowHDI_child*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])
    point$pathogen_urb_dif[a[j]]<-HumanData$dif_urb[a[j]]*UrbanDiffuse2Water*(ExcretionLowHDI_child*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])
    point$pathogen_rur_dif[a[j]]<-HumanData$dif_rur[a[j]]*RuralDiffuse2Water*(ExcretionLowHDI_child*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])
    point$pathogen_urb_onsite_treated[a[j]]<-HumanData$onsite_urb_treated[a[j]]*HumanData$survival_pit_empty_urb[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])/HumanData$storage_empty_urb[a[j]]
    point$pathogen_rur_onsite_treated[a[j]]<-HumanData$onsite_rur_treated[a[j]]*HumanData$survival_pit_empty_rur[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])/HumanData$storage_empty_rur[a[j]]
    point$pathogen_urb_onsite_water[a[j]]<-HumanData$onsite_urb_water[a[j]]*HumanData$survival_pit_empty_urb[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])/HumanData$storage_empty_urb[a[j]]
    point$pathogen_rur_onsite_water[a[j]]<-HumanData$onsite_rur_water[a[j]]*HumanData$survival_pit_empty_rur[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])/HumanData$storage_empty_rur[a[j]]
    point$pathogen_urb_onsite_land[a[j]]<-HumanData$onsite_urb_land[a[j]]*HumanData$survival_pit_empty_urb[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])/HumanData$storage_empty_urb[a[j]]
    point$pathogen_rur_onsite_land[a[j]]<-HumanData$onsite_rur_land[a[j]]*HumanData$survival_pit_empty_rur[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])/HumanData$storage_empty_rur[a[j]]
    point$pathogen_urb_onsite_flush[a[j]]<-HumanData$onsite_urb_flush[a[j]]*HumanData$survival_pit_flush_urb[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_urb_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_urb_over5[a[j]])/HumanData$storage_flush_urb[a[j]]
    point$pathogen_rur_onsite_flush[a[j]]<-HumanData$onsite_rur_flush[a[j]]*HumanData$survival_pit_flush_rur[a[j]]*UrbanOnsite2Water*(ExcretionLowHDI_child*NappyCor*HumanData$pop_rur_under5[a[j]]+ExcretionLowHDI_others*HumanData$pop_rur_over5[a[j]])/HumanData$storage_flush_rur[a[j]]

    if(wwtp_available==3){

      #In this case all emissions from the connected population are not yet included in the emissions to water. These are added to the grids in the next step. Treatment has not been applied yet.
      point$pathogen_urb_conforgrid[a[j]]<-point$pathogen_urb_con[a[j]]+point$pathogen_urb_onsite_treated[a[j]]
      point$pathogen_rur_conforgrid[a[j]]<-point$pathogen_rur_con[a[j]]+point$pathogen_rur_onsite_treated[a[j]]
      point$pathogen_urb_waterforgrid[a[j]]<-point$pathogen_urb_dir[a[j]]+point$pathogen_urb_onsite_water[a[j]]+point$pathogen_urb_onsite_flush[a[j]]
      point$pathogen_rur_waterforgrid[a[j]]<-point$pathogen_rur_dir[a[j]]+point$pathogen_rur_onsite_water[a[j]]+point$pathogen_rur_onsite_flush[a[j]]
      point$pathogen_urb_landforgrid[a[j]]<-point$pathogen_urb_dif[a[j]]+point$pathogen_urb_onsite_land[a[j]]
      point$pathogen_rur_landforgrid[a[j]]<-point$pathogen_rur_dif[a[j]]+point$pathogen_rur_onsite_land[a[j]]

    }else{

      #In this case all emissions from the connected population go to the water after treatment.
      point$pathogen_urb_conforgrid[a[j]]<-NA
      point$pathogen_rur_conforgrid[a[j]]<-NA
      point$pathogen_urb_waterforgrid[a[j]]<-point$pathogen_urb_con[a[j]]*HumanData$removalfraction[a[j]]+point$pathogen_urb_dir[a[j]]+point$pathogen_urb_onsite_treated[a[j]]*HumanData$removalfraction[a[j]]+point$pathogen_urb_onsite_water[a[j]]+point$pathogen_urb_onsite_flush[a[j]]
      point$pathogen_rur_waterforgrid[a[j]]<-point$pathogen_rur_con[a[j]]*HumanData$removalfraction[a[j]]+point$pathogen_rur_dir[a[j]]+point$pathogen_rur_onsite_treated[a[j]]*HumanData$removalfraction[a[j]]+point$pathogen_rur_onsite_water[a[j]]+point$pathogen_rur_onsite_flush[a[j]]
      point$pathogen_urb_landforgrid[a[j]]<-point$pathogen_urb_dif[a[j]]+point$pathogen_urb_onsite_land[a[j]]
      point$pathogen_rur_landforgrid[a[j]]<-point$pathogen_rur_dif[a[j]]+point$pathogen_rur_onsite_land[a[j]]

      #to make sure these fractions are added correctly to the output file
      point$pathogen_urb_con[a[j]]<-point$pathogen_urb_con[a[j]]*HumanData$removalfraction[a[j]]
      point$pathogen_rur_con[a[j]]<-point$pathogen_rur_con[a[j]]*HumanData$removalfraction[a[j]]

    }
  }
  rm(j)
}


# End to calculation of emissions per sanitation type and subregion -------


# Preparation for the distribution over the grids -------------------------

#Calculating average emission per head of the population (total population, not just under 5s, needed for compatibility with density ranking later)

for (j in 1:length(HumanData$iso)){

  point$pathogen_urb_waterforgrid_pp[j]<-point$pathogen_urb_waterforgrid[j]/HumanData$pop_urb_num[j]
  point$pathogen_rur_waterforgrid_pp[j]<-point$pathogen_rur_waterforgrid[j]/HumanData$pop_rur_num[j]

  point$pathogen_urb_landforgrid_pp[j]<-point$pathogen_urb_landforgrid[j]/HumanData$pop_urb_num[j]
  point$pathogen_rur_landforgrid_pp[j]<-point$pathogen_rur_landforgrid[j]/HumanData$pop_rur_num[j]

}

rm(j)

#remove NA from the point dataframe to prevent problems later on.
a<-which(is.na(point$pathogen_urb_conforgrid))
point$pathogen_urb_conforgrid[a]<-0
a<-which(is.na(point$pathogen_rur_conforgrid))
point$pathogen_rur_conforgrid[a]<-0
a<-which(is.na(point$pathogen_urb_waterforgrid))
point$pathogen_urb_waterforgrid[a]<-0
a<-which(is.na(point$pathogen_rur_waterforgrid))
point$pathogen_rur_waterforgrid[a]<-0
a<-which(is.na(point$pathogen_urb_landforgrid))
point$pathogen_urb_landforgrid[a]<-0
a<-which(is.na(point$pathogen_rur_landforgrid))
point$pathogen_rur_landforgrid[a]<-0
a<-which(is.na(point$pathogen_urb_con))
point$pathogen_urb_con[a]<-0
a<-which(is.na(point$pathogen_rur_con))
point$pathogen_rur_con[a]<-0
a<-which(is.na(point$pathogen_urb_onsite_land))
point$pathogen_urb_onsite_land[a]<-0
a<-which(is.na(point$pathogen_rur_onsite_land))
point$pathogen_rur_onsite_land[a]<-0
a<-which(is.na(point$pathogen_urb_dif))
point$pathogen_urb_dif[a]<-0
a<-which(is.na(point$pathogen_rur_dif))
point$pathogen_rur_dif[a]<-0

#estimate fraction of connected and land emissions for the different systems to later do source tracking

point$pathogen_urb_fconsewer<-0
point$pathogen_rur_fconsewer<-0
point$pathogen_urb_flandonsite<-0
point$pathogen_rur_flandonsite<-0
point$pathogen_urb_flanddiffuse<-0
point$pathogen_rur_flanddiffuse<-0

a<-which(point$pathogen_urb_conforgrid>0)
b<-which(point$pathogen_rur_conforgrid>0)
d<-which(point$pathogen_urb_landforgrid+point$pathogen_rur_landforgrid>0)

point$pathogen_urb_fconsewer[a]<-point$pathogen_urb_con[a]/point$pathogen_urb_conforgrid[a]
point$pathogen_rur_fconsewer[b]<-point$pathogen_rur_con[b]/point$pathogen_rur_conforgrid[b]
point$pathogen_urb_flandonsite[d]<-point$pathogen_urb_onsite_land[d]/(point$pathogen_urb_landforgrid[d]+point$pathogen_rur_landforgrid[d])
point$pathogen_rur_flandonsite[d]<-point$pathogen_rur_onsite_land[d]/(point$pathogen_urb_landforgrid[d]+point$pathogen_rur_landforgrid[d])
point$pathogen_urb_flanddiffuse[d]<-point$pathogen_urb_dif[d]/(point$pathogen_urb_landforgrid[d]+point$pathogen_rur_landforgrid[d])
point$pathogen_rur_flanddiffuse[d]<-point$pathogen_rur_dif[d]/(point$pathogen_urb_landforgrid[d]+point$pathogen_rur_landforgrid[d])

#this means: fcononsite=1-fconsewer and flanddirect=1-flandonsite

#previously we were saving the results to the human inputs here. We cannot do that now, because we cannot include the
#runoff of the land. Therefore this is moved to the next section, after the runoff.
#write.csv(point, file=paste0(working.path.out,"HumanEmissionsCalculated_",pathogen,"_",i,".csv"))

# End to preparation for spatial distribution -----------------------------


# Spatial distribution in case WWTP data are available --------------------

#Spatial distribution

##from population to pathogens
#for connected emissions in the case the location of WWTPs is known
if(wwtp_available ==3){

  #ASSUMPTION: the current capacity is exactly sufficient for the current waste - bypass is corrected for in the removal
  #rate (fraction no treatment in general calculation and otherwise in the sketcher tool)
  #THIS DEFINITELY IS SOMETHING TO LOOK INTO IN MORE DETAIL

  if(wwtp_available==1){
    WWTP_inputs_temp<-array(dim=c(0,5))
    colnames(WWTP_inputs_temp)<-c("Lon","Lat","subarea_name","Capacity","treatment_type")
    WWTP_inputs<-as.data.frame(WWTP_inputs_temp)
  }

  #find the subarea in which the grid is located
  WWTP_inputs$subarea<-raster::extract(isoraster,matrix(data=c(WWTP_inputs$Lon,WWTP_inputs$Lat),nrow=length(WWTP_inputs$Lon),ncol=2))
 
  #find the HDI for this subarea
  #removal rate of the WWTP is set to the removal rate of the district, only when this rate is not specified in the input file
  #MAKE SURE THAT THE REMOVAL RATE FROM THE SKETCHER IS IN FRACTION RATHER THAN LOG UNIT! IF LOG REMOVAL, CORRECT BY fred<-1-10^(-logremoval)

  for(j in 1:length(WWTP_inputs$subarea)){
#    WWTP_inputs$hdi[j]<-HumanData$hdi[which(HumanData$iso==WWTP_inputs$subarea[j])]
#    if(is.na(WWTP_inputs$removalfraction_sewer[j])){ #if the removal fraction has not been specified, then use the data from the district, otherwise the data is Matt's data, or estimated with the sketcher tool
#      WWTP_inputs$removalfraction_sewer[j]<-HumanData$removalfraction[which(HumanData$iso==WWTP_inputs$subarea[j])]
#    }
    
    if(identical(WWTP_inputs$treatment_type[j],"primary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen_inputs$RemovalPrimary[pathogen_row]
      WWTP_inputs$removalfraction_onsite[j]<-pathogen_inputs$RemovalPrimary[pathogen_row]
    }else if(identical(WWTP_inputs$treatment_type[j],"secondary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen_inputs$RemovalSecondary[pathogen_row]
      WWTP_inputs$removalfraction_onsite[j]<-pathogen_inputs$RemovalSecondary[pathogen_row]
    }else if(identical(WWTP_inputs$treatment_type[j],"tertiary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen_inputs$RemovalTertiary[pathogen_row]
      WWTP_inputs$removalfraction_onsite[j]<-pathogen_inputs$RemovalTertiary[pathogen_row]
    }else if(identical(WWTP_inputs$treatment_type[j],"quaternary")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen_inputs$RemovalQuaternary[pathogen_row]
      WWTP_inputs$removalfraction_onsite[j]<-pathogen_inputs$RemovalQuaternary[pathogen_row]
    }else if(identical(WWTP_inputs$treatment_type[j],"ponds")){
      WWTP_inputs$removalfraction_sewer[j]<-pathogen_inputs$RemovalPonds[pathogen_row]
      WWTP_inputs$removalfraction_onsite[j]<-pathogen_inputs$RemovalPonds[pathogen_row]
    }else if(identical(WWTP_inputs$treatment_type[j],"not treated")){
      WWTP_inputs$removalfraction_sewer[j]<-0
      WWTP_inputs$removalfraction_onsite[j]<-0
    }
    
#    if(is.na(WWTP_inputs$removalfraction_onsite[j])){
#      WWTP_inputs$removalfraction_onsite[j]<-HumanData$removalfraction[which(HumanData$iso==WWTP_inputs$subarea[j])]
#    }

  }

  #estimate the fraction of people connected per capacity unit
  capacity<-sum(WWTP_inputs$Capacity,na.rm=TRUE)  #how to deal with capacity is NA -> WWTP non existing?
  WWTP_inputs$fcapacity<-WWTP_inputs$Capacity/capacity

  #calculate the emissions per WWTP
  #the removal file for the WWTP is used if specified from the Sketcher tool, or the removal rate from the
  #literature is used (specified in for loop above)
  WWTP_inputs$emissions_per_WWTP_sewer_urb<-WWTP_inputs$fcapacity*(sum((point$pathogen_urb_conforgrid*point$pathogen_urb_fconsewer),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_sewer)
  WWTP_inputs$emissions_per_WWTP_sewer_rur<-WWTP_inputs$fcapacity*(sum((point$pathogen_rur_conforgrid*point$pathogen_rur_fconsewer),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_sewer)
  WWTP_inputs$emissions_per_WWTP_onsite_urb<-WWTP_inputs$fcapacity*(sum((point$pathogen_urb_conforgrid*(1-point$pathogen_urb_fconsewer)),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_onsite)
  WWTP_inputs$emissions_per_WWTP_onsite_rur<-WWTP_inputs$fcapacity*(sum((point$pathogen_rur_conforgrid*(1-point$pathogen_rur_fconsewer)),na.rm=TRUE))*(1-WWTP_inputs$removalfraction_onsite)
  WWTP_inputs$emissions_per_WWTP<-WWTP_inputs$emissions_per_WWTP_sewer_urb+WWTP_inputs$emissions_per_WWTP_sewer_rur+WWTP_inputs$emissions_per_WWTP_onsite_urb+WWTP_inputs$emissions_per_WWTP_onsite_rur

  print("Warning: currently average treatment coverage and efficiency are used. No link is yet established with the pathogen flow tool")

  WWTP_emissions_per_subarea<-array(dim=c(length(unique(WWTP_inputs$subarea)),5))
  colnames(WWTP_emissions_per_subarea)<-c("subarea","sewer_urb","sewer_rur","onsite_urb","onsite_rur")
  WWTP_emissions_per_subarea<-as.data.frame(WWTP_emissions_per_subarea)

  for(j in 1:length(unique(WWTP_inputs$subarea))){
    temp<-which(WWTP_inputs$subarea==unique(WWTP_inputs$subarea)[j])
    WWTP_emissions_per_subarea$subarea[j]<-WWTP_inputs$subarea[temp[1]]
    WWTP_emissions_per_subarea$sewer_urb[j]<-sum(WWTP_inputs$emissions_per_WWTP_sewer_urb[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$sewer_rur[j]<-sum(WWTP_inputs$emissions_per_WWTP_sewer_rur[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$onsite_urb[j]<-sum(WWTP_inputs$emissions_per_WWTP_onsite_urb[temp],na.rm=TRUE)
    WWTP_emissions_per_subarea$onsite_rur[j]<-sum(WWTP_inputs$emissions_per_WWTP_onsite_rur[temp],na.rm=TRUE)
  }

  point$pathogen_urb_WWTP_sewer<-0
  point$pathogen_rur_WWTP_sewer<-0
  point$pathogen_urb_WWTP_onsite<-0
  point$pathogen_rur_WWTP_onsite<-0

  for(j in 1:length(WWTP_emissions_per_subarea$subarea)){
    point$pathogen_urb_WWTP_sewer[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_urb[j]
    point$pathogen_rur_WWTP_sewer[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$sewer_rur[j]
    point$pathogen_urb_WWTP_onsite[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_urb[j]
    point$pathogen_rur_WWTP_onsite[which(HumanData$iso==WWTP_emissions_per_subarea$subarea[j])]<-WWTP_emissions_per_subarea$onsite_rur[j]
  }

  #now add the value to the correct grid in a new raster.
  pathogen_treatment<-isoraster
  pathogen_treatment[]<-0
  pathogen_treatment[cellFromXY(isoraster,matrix(data=c(WWTP_inputs$Lon,WWTP_inputs$Lat),nrow=length(WWTP_inputs$Lon),ncol=2))]<-WWTP_inputs$emissions_per_WWTP

  #for emissions to water
  pathogen_urban_water_pp<-data.frame(iso=point$iso,value=point$pathogen_urb_waterforgrid_pp)
  pathogen_urban_water_pp_raster<-subs(isoraster, pathogen_urban_water_pp , subsWithNA=T)

  pathogen_rural_water_pp<-data.frame(iso=point$iso,value=point$pathogen_rur_waterforgrid_pp)
  pathogen_rural_water_pp_raster<-subs(isoraster, pathogen_rural_water_pp , subsWithNA=T)

  pathogen_urban_water_grid<-pathogen_urban_water_pp_raster*popurban_grid
  pathogen_rural_water_grid<-pathogen_rural_water_pp_raster*poprural_grid

  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_water_grid<-subs(pathogen_urban_water_grid,temp,subsWithNA=F)
  pathogen_rural_water_grid<-subs(pathogen_rural_water_grid,temp,subsWithNA=F)

  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_water_grid<-subs(pathogen_urban_water_grid,temp,subsWithNA=F)
  pathogen_rural_water_grid<-subs(pathogen_rural_water_grid,temp,subsWithNA=F)

  #also add the pathogens left over after treatment to the water emissions
  pathogen_water_grid<-pathogen_urban_water_grid+pathogen_rural_water_grid+pathogen_treatment

  #for emissions to land
  pathogen_urban_land_pp<-data.frame(iso=point$iso,value=point$pathogen_urb_landforgrid_pp)
  pathogen_urban_land_pp_raster<-subs(isoraster, pathogen_urban_land_pp , subsWithNA=T)

  pathogen_rural_land_pp<-data.frame(iso=point$iso,value=point$pathogen_rur_landforgrid_pp)
  pathogen_rural_land_pp_raster<-subs(isoraster, pathogen_rural_land_pp , subsWithNA=T)

  pathogen_urban_land_grid<-pathogen_urban_land_pp_raster*popurban_grid
  pathogen_rural_land_grid<-pathogen_rural_land_pp_raster*poprural_grid

  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)

  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)

  pathogen_land_grid<-pathogen_urban_land_grid+pathogen_rural_land_grid


# End to spatial distribution in case WWTP data available -----------------


# Start to spatial distribution in case specific WWTP data unavailable -------------

} else{

  #emissions to WWTP are not separated and already included in the emissions to water
  #for emissions to water
  pathogen_urban_water_pp<-data.frame(iso=point$iso,value=point$pathogen_urb_waterforgrid_pp)
  pathogen_urban_water_pp_raster<-subs(isoraster, pathogen_urban_water_pp , subsWithNA=T)

  pathogen_rural_water_pp<-data.frame(iso=point$iso,value=point$pathogen_rur_waterforgrid_pp)
  pathogen_rural_water_pp_raster<-subs(isoraster, pathogen_rural_water_pp , subsWithNA=T)

  pathogen_urban_water_grid<-pathogen_urban_water_pp_raster*popurban_grid
  pathogen_rural_water_grid<-pathogen_rural_water_pp_raster*poprural_grid

  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_water_grid<-subs(pathogen_urban_water_grid,temp,subsWithNA=F)
  pathogen_rural_water_grid<-subs(pathogen_rural_water_grid,temp,subsWithNA=F)

  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_water_grid<-subs(pathogen_urban_water_grid,temp,subsWithNA=F)
  pathogen_rural_water_grid<-subs(pathogen_rural_water_grid,temp,subsWithNA=F)

  pathogen_water_grid<-pathogen_urban_water_grid+pathogen_rural_water_grid

  #for emissions to land
  pathogen_urban_land_pp<-data.frame(iso=point$iso,value=point$pathogen_urb_landforgrid_pp)
  pathogen_urban_land_pp_raster<-subs(isoraster, pathogen_urban_land_pp , subsWithNA=T)

  pathogen_rural_land_pp<-data.frame(iso=point$iso,value=point$pathogen_rur_landforgrid_pp)
  pathogen_rural_land_pp_raster<-subs(isoraster, pathogen_rural_land_pp , subsWithNA=T)

  pathogen_urban_land_grid<-pathogen_urban_land_pp_raster*popurban_grid
  pathogen_rural_land_grid<-pathogen_rural_land_pp_raster*poprural_grid

  temp<-data.frame(bla=NA,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)

  temp<-data.frame(bla=NaN,value=0)
  pathogen_urban_land_grid<-subs(pathogen_urban_land_grid,temp,subsWithNA=F)
  pathogen_rural_land_grid<-subs(pathogen_rural_land_grid,temp,subsWithNA=F)

  pathogen_land_grid<-pathogen_urban_land_grid+pathogen_rural_land_grid

}


# End to spatial distribution in case specific WWTP data is unavailable --------


# Correcting the earlier output file for WWTP removal ---------------------

#make sure the output file puts the connected fraction in the right district, applying treatment also when
#wwtpdata is available.

if(wwtp_available==3){
  point$pathogen_urb_con<-point$pathogen_urb_WWTP_sewer
  point$pathogen_rur_con<-point$pathogen_rur_WWTP_sewer
  point$pathogen_urb_onsite_treated<-point$pathogen_urb_WWTP_onsite
  point$pathogen_rur_onsite_treated<-point$pathogen_rur_WWTP_onsite
}


# End to correction earlier output file -----------------------------------


# Saving file for later use with hydrology - not currently used -----------

if(isTRUE(hydrology_available)){
  writeRaster(pathogen_land_grid,paste0(working.path.in,"modeloutput_humans/humanemissions_diffuse_",pathogen,i), format="ascii", overwrite=TRUE)
  writeRaster(pathogen_water_grid,paste0(working.path.in,"modeloutput_humans/humanemissions_",pathogen,i), format="ascii", overwrite=TRUE)

}else{
  

# End to saving file for later use with hydrology -------------------------

# Applying retention on land and write raster ----------------------------------------------

  #In case no hydrology is available, we assume a 0.025 fraction reaching the water. This is high for many regions!

  pathogen_land_grid<-0.025*pathogen_land_grid
  pathogen_water_grid<-pathogen_water_grid+pathogen_land_grid
  point$pathogen_urb_dif<-0.025*point$pathogen_urb_dif
  point$pathogen_rur_dif<-0.025*point$pathogen_rur_dif
  point$pathogen_urb_onsite_land<-0.025*point$pathogen_urb_onsite_land
  point$pathogen_rur_onsite_land<-0.025*point$pathogen_rur_onsite_land

  writeRaster(pathogen_water_grid,paste0(working.path.out,"humanemissions_",pathogen,run), format="ascii", overwrite=TRUE)
  

# End to applying retention on land and write raster ----------------------


# Plotting ----------------------------------------------------------------
  # For the new tool, we do not need to plot .png files anymore, I think. We only require geotiffs.

  #Make .png plot (mostly only relevant for the tool and for a quick look at the results)
  colors<-c("slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")
  pathogen_water_grid[pathogen_water_grid==0]<-NA
  log_pathogen_water_grid<-log10(pathogen_water_grid)
  mx<-max(log_pathogen_water_grid[],na.rm=TRUE)
  mn<-min(log_pathogen_water_grid[],na.rm=TRUE)
  brks<-c(floor(mn),floor(mn)+(ceiling(mx)-floor(mn))/10,floor(mn)+2*(ceiling(mx)-floor(mn))/10,floor(mn)+3*(ceiling(mx)-floor(mn))/10,floor(mn)+4*(ceiling(mx)-floor(mn))/10,floor(mn)+5*(ceiling(mx)-floor(mn))/10,floor(mn)+6*(ceiling(mx)-floor(mn))/10,floor(mn)+7*(ceiling(mx)-floor(mn))/10,floor(mn)+8*(ceiling(mx)-floor(mn))/10,floor(mn)+9*(ceiling(mx)-floor(mn))/10,ceiling(mx))

  #plot Png file
  filename_png<-paste0(working.path.out,"humanemissions_",pathogen,run,".png")
  png(filename=filename_png,width=750,height=750,units="px")
  par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n")
  plot(log_pathogen_water_grid, col=colors, breaks=brks,legend=FALSE, axes=FALSE)

  title(main=paste0("Human emissions per grid baseline run"))

  #if the resolution is 0.08333 or 0.008333, the district data or higher res need to be plotted, if the resolution is 0.5, the country data needs to be plotted

  if(reso == 0.5){
    plot(countriesHigh, add=TRUE)
  }else{
    setwd(paste0(working.path.in,"districts_shapefile/"))
    boundaries<-readOGR(dsn = ".", layer = overall_inputs$shapefile_filename)
    boundaries<-spTransform(boundaries,CRS("+proj=longlat +datum=WGS84"))
    plot(boundaries,add=TRUE) #overall_inputs$shapefile_filename[i]
    setwd<-wd
  }

  if(pathogen=="cryptosporidium"){
    plot(log_pathogen_water_grid, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
         legend.width=1, legend.shrink=0.75,
         axis.args=list(at=brks,
                        labels=brks,
                        cex.axis=1,font=1.4),
         legend.args=list(text='Cryptosporidium emissions (log10 oocysts / year)', font=1.8, cex=1.2))
  }else if(pathogen=="rotavirus"){
    plot(log_pathogen_water_grid, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
         legend.width=1, legend.shrink=0.75,
         axis.args=list(at=brks,
                        labels=brks,
                        cex.axis=1,font=1.4),
         legend.args=list(text='Rotavirus emissions (log10 viral particles / year)', font=1.8, cex=1.2))
  }
  dev.off()

  #end of plotting .png file

  writeRaster(pathogen_water_grid,paste0(working.path.out,"humanemissions_",pathogen,run),format="GTiff",overwrite=TRUE)
  

# End to plotting ---------------------------------------------------------


# Save the output .csv file and remove the variables ----------------------

  library(tidyverse)
  library(dplyr)

  point1<-point %>% select(1:16)
  write.csv(point1, file=paste0(working.path.out,"HumanEmissionsCalculated_",pathogen,"_",run,".csv"))

  detach("package:tidyverse", unload=TRUE)
  #  .rs.unloadPackage("tidyverse")
  detach("package:dplyr", unload=TRUE)
#  .rs.unloadPackage("dplyr")
  detach("package:dplyr", unload=TRUE)
#  .rs.unloadPackage("tidyr")

  print("As no hydrology data is available, we use a fraction of 0.025 of the emissions on the land that reach the water in the grid.")
}

toremove <- grep("^point$|^HumanData$|^results$|^i$|^isoraster$|^overall_inputs$|^pathogen_inputs$|^area_extent$|^hydrology_available$|^interest_concentrations|^livestock_switch$|^mons$|^pathogen$|^pathogen_row$|^reso$|^runs$|^run_number$|^wd$|^working.path|^isoraster_hydro$|run", ls(),
                 invert = TRUE,
                 value = TRUE)
rm(list = c(toremove, "toremove"))

#Plotting can be done from the emissions files. Later on (after runoff) also the contribution of the diffuse emissions will be available.
#The emissions per source are saved later (after runoff). Until then the dataframe point should not be removed.
