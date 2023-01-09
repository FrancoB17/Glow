rm(list=ls())

years<-c(2010, 2020, 2030, 2050, 2070)
scenarios<-c("BAU","High","Low", "Max", "Moderate") 

for (l in 1:length(years)){
for (m in 1:length(scenarios)){

year<-years[l]
scenario<-scenarios[m]

library(readxl)

working.path.in<-"C:/Users/hofst023/OneDrive - Wageningen University & Research/Oude D schijf/Onderwijs/Afstudeervakken/Anna Sophie Braun/Model files/Scenarios Peter/"
file<-paste0("SSP2_",scenario,"_modelinput.xlsx")
file<-paste0(working.path.in,file)

#read in data from Peter

read_excel_allsheets <- function(filename, tibble = FALSE,skip) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,skip=skip))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

data <- read_excel_allsheets(file,skip=3)

#read in original input file, which is the basis for the new one
reference_data<-read.csv("C:/Users/hofst023/OneDrive - Wageningen University & Research/Oude D schijf/Onderzoek/K2P/test/glowpa-model/Model input/input_file_world_country_20201119.csv")

#make an array with the order of the reference_data, containing the numbers of the line where the subarea matches
volgorde<-data.frame(array(data=NA, dim = length(reference_data$iso)))
volgorde$ints<-NA

#some countries have different spellings. I only selected relevant ones, not small island states.
temp<-c(35,45,47,48,117,123,125,128,145,149,182,190,205,206,223,229,242)
temp1<-c(24,89,42,41,92,96,99,104,187,199,95,149,199,169,144,38,162)

for (i in 1:length(reference_data$iso)){
 a<-which(data$popnum$Country==reference_data$subarea[i])
 if(length(a)>0){
   volgorde$ints[i]<-a
 }else{
   b<-which(temp==i)
   if(length(b)>0){
     volgorde$ints[i]<-temp1[b]
   }else{
     volgorde$ints[i]<-NA 
   }
 }
} 

#for the reference data determine the fractions used for pits and unimproved (urban and rural the same)
#pits include flush pit, pit with slab, composting toilet
#unimproved includes open defecation, hanging toilets, container based, pit without slab, bucket latrine, 
#    flush open, flush unknown and other

#NA and NaN results now 0 - do fractions in the end still add up? CHECK!

fflushPit_urb<-reference_data$flushPit_urb/(reference_data$flushPit_urb+reference_data$pitSlab_urb+reference_data$compostingToilet_urb)
fpitSlab_urb<-reference_data$pitSlab_urb/(reference_data$flushPit_urb+reference_data$pitSlab_urb+reference_data$compostingToilet_urb)
fcompostingToilet_urb<-reference_data$compostingToilet_urb/(reference_data$flushPit_urb+reference_data$pitSlab_urb+reference_data$compostingToilet_urb)

fflushPit_rur<-reference_data$flushPit_rur/(reference_data$flushPit_rur+reference_data$pitSlab_rur+reference_data$compostingToilet_rur)
fpitSlab_rur<-reference_data$pitSlab_rur/(reference_data$flushPit_rur+reference_data$pitSlab_rur+reference_data$compostingToilet_rur)
fcompostingToilet_rur<-reference_data$compostingToilet_rur/(reference_data$flushPit_rur+reference_data$pitSlab_rur+reference_data$compostingToilet_rur)

fflushPit_urb[is.na(fflushPit_urb)]<-0
fpitSlab_urb[is.na(fpitSlab_urb)]<-0
fcompostingToilet_urb[is.na(fcompostingToilet_urb)]<-0

fflushPit_rur[is.na(fflushPit_rur)]<-0
fpitSlab_rur[is.na(fpitSlab_rur)]<-0
fcompostingToilet_rur[is.na(fcompostingToilet_rur)]<-0

fflushOpen_urb<-reference_data$flushOpen_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fflushUnknown_urb<-reference_data$flushUnknown_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fpitNoSlab_urb<-reference_data$pitNoSlab_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fbucketLatrine_urb<-reference_data$bucketLatrine_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fcontainerBased_urb<-reference_data$containerBased_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fhangingToilet_urb<-reference_data$hangingToilet_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fopenDefecation_urb<-reference_data$openDefecation_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)
fother_urb<-reference_data$other_urb/(reference_data$flushOpen_urb+reference_data$flushUnknown_urb+reference_data$pitNoSlab_urb+reference_data$bucketLatrine_urb+reference_data$containerBased_urb+reference_data$hangingToilet_urb+reference_data$openDefecation_urb+reference_data$other_urb)

fflushOpen_rur<-reference_data$flushOpen_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fflushUnknown_rur<-reference_data$flushUnknown_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fpitNoSlab_rur<-reference_data$pitNoSlab_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fbucketLatrine_rur<-reference_data$bucketLatrine_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fcontainerBased_rur<-reference_data$containerBased_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fhangingToilet_rur<-reference_data$hangingToilet_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fopenDefecation_rur<-reference_data$openDefecation_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)
fother_rur<-reference_data$other_rur/(reference_data$flushOpen_rur+reference_data$flushUnknown_rur+reference_data$pitNoSlab_rur+reference_data$bucketLatrine_rur+reference_data$containerBased_rur+reference_data$hangingToilet_rur+reference_data$openDefecation_rur+reference_data$other_rur)

fflushOpen_urb[is.na(fother_urb)]<-0
fflushUnknown_urb[is.na(fflushUnknown_urb)]<-0
fpitNoSlab_urb[is.na(fpitNoSlab_urb)]<-0
fbucketLatrine_urb[is.na(fbucketLatrine_urb)]<-0
fcontainerBased_urb[is.na(fcontainerBased_urb)]<-0
fhangingToilet_urb[is.na(fhangingToilet_urb)]<-0
fopenDefecation_urb[is.na(fopenDefecation_urb)]<-0
fother_urb[is.na(fother_urb)]<-0

fflushOpen_rur[is.na(fother_rur)]<-0
fflushUnknown_rur[is.na(fflushUnknown_rur)]<-0
fpitNoSlab_rur[is.na(fpitNoSlab_rur)]<-0
fbucketLatrine_rur[is.na(fbucketLatrine_rur)]<-0
fcontainerBased_rur[is.na(fcontainerBased_rur)]<-0
fhangingToilet_rur[is.na(fhangingToilet_rur)]<-0
fopenDefecation_rur[is.na(fopenDefecation_rur)]<-0
fother_rur[is.na(fother_rur)]<-0

#now set the scenario file and for each country transfer HDI, population and fraction urban
scenario_data<-reference_data
scenario_data[5:7]<-NA
scenario_data[21:77]<-NA

hdi<-read.csv(paste0(working.path.in,"HDI.csv"))

q<-which(hdi$ISO.CODE!=data$popnum$`ISO-CODE`)
if(length(q)>0){
  print("ERROR, iso not the same!")
}
q<-which(data$popurb$`ISO-CODE`!= data$popnum$`ISO-CODE`)
if(length(q)>0){
  print("ERROR, iso not the same!")
}

for (i in 1: length(reference_data$iso)){
  if(year==2010){
    scenario_data$hdi[i]<-hdi$X2010[volgorde$ints[i]]
    scenario_data$population[i]<-data$popnum$'2010'[volgorde$ints[i]]*1000
    scenario_data$fraction_urban_pop[i]<-data$popurb$'2010'[volgorde$ints[i]]
  }else if (year==2020){
    scenario_data$hdi[i]<-hdi$X2020[volgorde$ints[i]]
    scenario_data$population[i]<-data$popnum$'2020'[volgorde$ints[i]]*1000
    scenario_data$fraction_urban_pop[i]<-data$popurb$'2020'[volgorde$ints[i]]
  }else if (year==2030){
    scenario_data$hdi[i]<-hdi$X2030[volgorde$ints[i]]
    scenario_data$population[i]<-data$popnum$'2030'[volgorde$ints[i]]*1000
    scenario_data$fraction_urban_pop[i]<-data$popurb$'2030'[volgorde$ints[i]]
  }else if (year==2050){
    scenario_data$hdi[i]<-hdi$X2050[volgorde$ints[i]]
    scenario_data$population[i]<-data$popnum$'2050'[volgorde$ints[i]]*1000
    scenario_data$fraction_urban_pop[i]<-data$popurb$'2050'[volgorde$ints[i]]
  }else if (year==2070){
    scenario_data$hdi[i]<-hdi$X2070[volgorde$ints[i]]
    scenario_data$population[i]<-data$popnum$'2070'[volgorde$ints[i]]*1000
    scenario_data$fraction_urban_pop[i]<-data$popurb$'2070'[volgorde$ints[i]]
  }
}

#for each country now transfer the sanitation data to the new scenario file

for(i in 1:length(reference_data$iso)){
  if(year==2010){
    if(!is.na(volgorde$ints[i])){
      scenario_data$flushSewer_urb[i]<-data$urb_sewr$'2010'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_urb[i]<-data$urb_sept$'2010'[volgorde$ints[i]]/100
      scenario_data$flushPit_urb[i]<-data$urb_latr$'2010'[volgorde$ints[i]]/100*fflushPit_urb[i]
      scenario_data$flushOpen_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fflushOpen_urb[i]
      scenario_data$flushUnknown_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fflushUnknown_urb[i]
      scenario_data$pitSlab_urb[i]<-data$urb_latr$'2010'[volgorde$ints[i]]/100*fpitSlab_urb[i]
      scenario_data$pitNoSlab_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fpitNoSlab_urb[i]
      scenario_data$compostingToilet_urb[i]<-data$urb_latr$'2010'[volgorde$ints[i]]/100*fcompostingToilet_urb[i]
      scenario_data$bucketLatrine_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fbucketLatrine_urb[i]
      scenario_data$containerBased_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fcontainerBased_urb[i]
      scenario_data$hangingToilet_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fhangingToilet_urb[i]
      scenario_data$openDefecation_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fopenDefecation_urb[i]
      scenario_data$other_urb[i]<-data$urb_unop$'2010'[volgorde$ints[i]]/100*fother_urb[i]
      
      scenario_data$flushSewer_rur[i]<-data$rur_sewr$'2010'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_rur[i]<-data$rur_sept$'2010'[volgorde$ints[i]]/100
      scenario_data$flushPit_rur[i]<-data$rur_latr$'2010'[volgorde$ints[i]]/100*fflushPit_rur[i]
      scenario_data$flushOpen_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fflushOpen_rur[i]
      scenario_data$flushUnknown_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fflushUnknown_rur[i]
      scenario_data$pitSlab_rur[i]<-data$rur_latr$'2010'[volgorde$ints[i]]/100*fpitSlab_rur[i]
      scenario_data$pitNoSlab_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fpitNoSlab_rur[i]
      scenario_data$compostingToilet_rur[i]<-data$rur_latr$'2010'[volgorde$ints[i]]/100*fcompostingToilet_rur[i]
      scenario_data$bucketLatrine_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fbucketLatrine_rur[i]
      scenario_data$containerBased_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fcontainerBased_rur[i]
      scenario_data$hangingToilet_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fhangingToilet_rur[i]
      scenario_data$openDefecation_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fopenDefecation_rur[i]
      scenario_data$other_rur[i]<-data$rur_unop$'2010'[volgorde$ints[i]]/100*fother_rur[i]
    }
  } else if (year==2020){
    if(!is.na(volgorde$ints[i])){
      scenario_data$flushSewer_urb[i]<-data$urb_sewr$'2020'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_urb[i]<-data$urb_sept$'2020'[volgorde$ints[i]]/100
      scenario_data$flushPit_urb[i]<-data$urb_latr$'2020'[volgorde$ints[i]]/100*fflushPit_urb[i]
      scenario_data$flushOpen_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fflushOpen_urb[i]
      scenario_data$flushUnknown_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fflushUnknown_urb[i]
      scenario_data$pitSlab_urb[i]<-data$urb_latr$'2020'[volgorde$ints[i]]/100*fpitSlab_urb[i]
      scenario_data$pitNoSlab_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fpitNoSlab_urb[i]
      scenario_data$compostingToilet_urb[i]<-data$urb_latr$'2020'[volgorde$ints[i]]/100*fcompostingToilet_urb[i]
      scenario_data$bucketLatrine_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fbucketLatrine_urb[i]
      scenario_data$containerBased_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fcontainerBased_urb[i]
      scenario_data$hangingToilet_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fhangingToilet_urb[i]
      scenario_data$openDefecation_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fopenDefecation_urb[i]
      scenario_data$other_urb[i]<-data$urb_unop$'2020'[volgorde$ints[i]]/100*fother_urb[i]
      
      scenario_data$flushSewer_rur[i]<-data$rur_sewr$'2020'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_rur[i]<-data$rur_sept$'2020'[volgorde$ints[i]]/100
      scenario_data$flushPit_rur[i]<-data$rur_latr$'2020'[volgorde$ints[i]]/100*fflushPit_rur[i]
      scenario_data$flushOpen_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fflushOpen_rur[i]
      scenario_data$flushUnknown_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fflushUnknown_rur[i]
      scenario_data$pitSlab_rur[i]<-data$rur_latr$'2020'[volgorde$ints[i]]/100*fpitSlab_rur[i]
      scenario_data$pitNoSlab_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fpitNoSlab_rur[i]
      scenario_data$compostingToilet_rur[i]<-data$rur_latr$'2020'[volgorde$ints[i]]/100*fcompostingToilet_rur[i]
      scenario_data$bucketLatrine_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fbucketLatrine_rur[i]
      scenario_data$containerBased_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fcontainerBased_rur[i]
      scenario_data$hangingToilet_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fhangingToilet_rur[i]
      scenario_data$openDefecation_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fopenDefecation_rur[i]
      scenario_data$other_rur[i]<-data$rur_unop$'2020'[volgorde$ints[i]]/100*fother_rur[i]
    }
  } else if (year==2030){
    if(!is.na(volgorde$ints[i])){
      scenario_data$flushSewer_urb[i]<-data$urb_sewr$'2030'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_urb[i]<-data$urb_sept$'2030'[volgorde$ints[i]]/100
      scenario_data$flushPit_urb[i]<-data$urb_latr$'2030'[volgorde$ints[i]]/100*fflushPit_urb[i]
      scenario_data$flushOpen_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fflushOpen_urb[i]
      scenario_data$flushUnknown_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fflushUnknown_urb[i]
      scenario_data$pitSlab_urb[i]<-data$urb_latr$'2030'[volgorde$ints[i]]/100*fpitSlab_urb[i]
      scenario_data$pitNoSlab_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fpitNoSlab_urb[i]
      scenario_data$compostingToilet_urb[i]<-data$urb_latr$'2030'[volgorde$ints[i]]/100*fcompostingToilet_urb[i]
      scenario_data$bucketLatrine_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fbucketLatrine_urb[i]
      scenario_data$containerBased_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fcontainerBased_urb[i]
      scenario_data$hangingToilet_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fhangingToilet_urb[i]
      scenario_data$openDefecation_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fopenDefecation_urb[i]
      scenario_data$other_urb[i]<-data$urb_unop$'2030'[volgorde$ints[i]]/100*fother_urb[i]
      
      scenario_data$flushSewer_rur[i]<-data$rur_sewr$'2030'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_rur[i]<-data$rur_sept$'2030'[volgorde$ints[i]]/100
      scenario_data$flushPit_rur[i]<-data$rur_latr$'2030'[volgorde$ints[i]]/100*fflushPit_rur[i]
      scenario_data$flushOpen_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fflushOpen_rur[i]
      scenario_data$flushUnknown_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fflushUnknown_rur[i]
      scenario_data$pitSlab_rur[i]<-data$rur_latr$'2030'[volgorde$ints[i]]/100*fpitSlab_rur[i]
      scenario_data$pitNoSlab_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fpitNoSlab_rur[i]
      scenario_data$compostingToilet_rur[i]<-data$rur_latr$'2030'[volgorde$ints[i]]/100*fcompostingToilet_rur[i]
      scenario_data$bucketLatrine_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fbucketLatrine_rur[i]
      scenario_data$containerBased_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fcontainerBased_rur[i]
      scenario_data$hangingToilet_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fhangingToilet_rur[i]
      scenario_data$openDefecation_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fopenDefecation_rur[i]
      scenario_data$other_rur[i]<-data$rur_unop$'2030'[volgorde$ints[i]]/100*fother_rur[i]
    }
  } else if (year==2050){
    if(!is.na(volgorde$ints[i])){
      scenario_data$flushSewer_urb[i]<-data$urb_sewr$'2050'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_urb[i]<-data$urb_sept$'2050'[volgorde$ints[i]]/100
      scenario_data$flushPit_urb[i]<-data$urb_latr$'2050'[volgorde$ints[i]]/100*fflushPit_urb[i]
      scenario_data$flushOpen_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fflushOpen_urb[i]
      scenario_data$flushUnknown_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fflushUnknown_urb[i]
      scenario_data$pitSlab_urb[i]<-data$urb_latr$'2050'[volgorde$ints[i]]/100*fpitSlab_urb[i]
      scenario_data$pitNoSlab_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fpitNoSlab_urb[i]
      scenario_data$compostingToilet_urb[i]<-data$urb_latr$'2050'[volgorde$ints[i]]/100*fcompostingToilet_urb[i]
      scenario_data$bucketLatrine_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fbucketLatrine_urb[i]
      scenario_data$containerBased_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fcontainerBased_urb[i]
      scenario_data$hangingToilet_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fhangingToilet_urb[i]
      scenario_data$openDefecation_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fopenDefecation_urb[i]
      scenario_data$other_urb[i]<-data$urb_unop$'2050'[volgorde$ints[i]]/100*fother_urb[i]
      
      scenario_data$flushSewer_rur[i]<-data$rur_sewr$'2050'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_rur[i]<-data$rur_sept$'2050'[volgorde$ints[i]]/100
      scenario_data$flushPit_rur[i]<-data$rur_latr$'2050'[volgorde$ints[i]]/100*fflushPit_rur[i]
      scenario_data$flushOpen_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fflushOpen_rur[i]
      scenario_data$flushUnknown_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fflushUnknown_rur[i]
      scenario_data$pitSlab_rur[i]<-data$rur_latr$'2050'[volgorde$ints[i]]/100*fpitSlab_rur[i]
      scenario_data$pitNoSlab_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fpitNoSlab_rur[i]
      scenario_data$compostingToilet_rur[i]<-data$rur_latr$'2050'[volgorde$ints[i]]/100*fcompostingToilet_rur[i]
      scenario_data$bucketLatrine_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fbucketLatrine_rur[i]
      scenario_data$containerBased_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fcontainerBased_rur[i]
      scenario_data$hangingToilet_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fhangingToilet_rur[i]
      scenario_data$openDefecation_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fopenDefecation_rur[i]
      scenario_data$other_rur[i]<-data$rur_unop$'2050'[volgorde$ints[i]]/100*fother_rur[i]
    }
  } else if (year==2070){
    if(!is.na(volgorde$ints[i])){
      scenario_data$flushSewer_urb[i]<-data$urb_sewr$'2070'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_urb[i]<-data$urb_sept$'2070'[volgorde$ints[i]]/100
      scenario_data$flushPit_urb[i]<-data$urb_latr$'2070'[volgorde$ints[i]]/100*fflushPit_urb[i]
      scenario_data$flushOpen_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fflushOpen_urb[i]
      scenario_data$flushUnknown_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fflushUnknown_urb[i]
      scenario_data$pitSlab_urb[i]<-data$urb_latr$'2070'[volgorde$ints[i]]/100*fpitSlab_urb[i]
      scenario_data$pitNoSlab_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fpitNoSlab_urb[i]
      scenario_data$compostingToilet_urb[i]<-data$urb_latr$'2070'[volgorde$ints[i]]/100*fcompostingToilet_urb[i]
      scenario_data$bucketLatrine_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fbucketLatrine_urb[i]
      scenario_data$containerBased_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fcontainerBased_urb[i]
      scenario_data$hangingToilet_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fhangingToilet_urb[i]
      scenario_data$openDefecation_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fopenDefecation_urb[i]
      scenario_data$other_urb[i]<-data$urb_unop$'2070'[volgorde$ints[i]]/100*fother_urb[i]
      
      scenario_data$flushSewer_rur[i]<-data$rur_sewr$'2070'[volgorde$ints[i]]/100 
      scenario_data$flushSeptic_rur[i]<-data$rur_sept$'2070'[volgorde$ints[i]]/100
      scenario_data$flushPit_rur[i]<-data$rur_latr$'2070'[volgorde$ints[i]]/100*fflushPit_rur[i]
      scenario_data$flushOpen_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fflushOpen_rur[i]
      scenario_data$flushUnknown_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fflushUnknown_rur[i]
      scenario_data$pitSlab_rur[i]<-data$rur_latr$'2070'[volgorde$ints[i]]/100*fpitSlab_rur[i]
      scenario_data$pitNoSlab_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fpitNoSlab_rur[i]
      scenario_data$compostingToilet_rur[i]<-data$rur_latr$'2070'[volgorde$ints[i]]/100*fcompostingToilet_rur[i]
      scenario_data$bucketLatrine_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fbucketLatrine_rur[i]
      scenario_data$containerBased_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fcontainerBased_rur[i]
      scenario_data$hangingToilet_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fhangingToilet_rur[i]
      scenario_data$openDefecation_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fopenDefecation_rur[i]
      scenario_data$other_rur[i]<-data$rur_unop$'2070'[volgorde$ints[i]]/100*fother_rur[i]
    }
  }
}

#adapt the management according to Tolga's table
#Assumptions:
#coverBury_urb
#sewageTreated_urb
#fecalSludgeTreated_urb
#isWatertight_urb
#hasLeach_urb
#emptyFrequency_urb
#pitAdditive_urb
#urine_urb
#twinPits_urb
#onsiteDumpedland_urb - 0.1 does not change

hdi_cutoffs<-c(0.42,0.56,0.63) #quartiles of scenario hdi data 2015

#put the turnover table in a dataframe
#word in 'title' is scenario, word in column title is management, order is low to high hdi category
septic_table_low_urb<-data.frame(low=c(0.5,0.3,0.1,0.1),med=c(0.4,0.4,0.2,0.1),high=c(0.1,0.3,0.7,0.8))
septic_table_med_urb<-data.frame(low=c(0.4,0.2,0,0),med=c(0.4,0.4,0.2,0.1),high=c(0.2,0.4,0.8,0.9))
septic_table_high_urb<-data.frame(low=c(0.3,0.1,0,0),med=c(0.4,0.4,0.1,0),high=c(0.3,0.5,0.9,1))
septic_table_low_rur<-data.frame(low=c(0.9,0.5,0.3,0.1),med=c(0.1,0.2,0.2,0.1),high=c(0,0.3,0.5,0.8))
septic_table_med_rur<-data.frame(low=c(0.8,0.4,0.2,0),med=c(0.2,0.2,0.2,0.1),high=c(0,0.4,0.6,0.9))
septic_table_high_rur<-data.frame(low=c(0.7,0.3,0.1,0),med=c(0.2,0.2,0.2,0),high=c(0.2,0.5,0.7,1))
pit_table_low_urb<-data.frame(low=c(0.4,0.3,0.3,0),med=c(0.5,0.4,0.2,0.1),high=c(0.1,0.3,0.5,0.9))
pit_table_med_urb<-data.frame(low=c(0.3,0.2,0.2,0),med=c(0.5,0.4,0.2,0),high=c(0.2,0.4,0.6,1))
pit_table_high_urb<-data.frame(low=c(0.2,0.1,0.1,0),med=c(0.5,0.4,0.2,0),high=c(0.3,0.5,0.7,1))
pit_table_low_rur<-data.frame(low=c(0.3,0.1,0.1,0),med=c(0.4,0.4,0.2,0.1),high=c(0.3,0.5,0.7,0.9))
pit_table_med_rur<-data.frame(low=c(0.2,0,0,0),med=c(0.4,0.4,0.2,0),high=c(0.4,0.6,0.8,1))
pit_table_high_rur<-data.frame(low=c(0.1,0,0,0),med=c(0.4,0.3,0.1,0),high=c(0.5,0.7,0.9,1))

if(year==2010){
  septic_table_urb<-septic_table_med_urb
  pit_table_urb<-pit_table_med_urb
  septic_table_rur<-septic_table_med_rur
  pit_table_rur<-pit_table_med_rur
}else{
  if(scenario=="BAU"|scenario=="Moderate"){
    septic_table_urb<-septic_table_med_urb
    pit_table_urb<-pit_table_med_urb
    septic_table_rur<-septic_table_med_rur
    pit_table_rur<-pit_table_med_rur
  }else if (scenario=="Low"){
    septic_table_urb<-septic_table_low_urb
    pit_table_urb<-pit_table_low_urb
    septic_table_rur<-septic_table_low_rur
    pit_table_rur<-pit_table_low_rur
  }else{ #scenario is High or Max
    septic_table_urb<-septic_table_high_urb
    pit_table_urb<-pit_table_high_urb
    septic_table_rur<-septic_table_high_rur
    pit_table_rur<-pit_table_high_rur
  }
}

#TO DO: check this bit of code!
for(i in 1:length(reference_data$iso)){
 
  scenario_data$emptyFrequency_urb[i]<-3
  scenario_data$emptyFrequency_rur[i]<-3
  scenario_data$pitAdditive_urb[i]<-0
  scenario_data$pitAdditive_rur[i]<-0
  scenario_data$urine_urb[i]<-0
  scenario_data$urine_rur[i]<-0
  scenario_data$twinPits_urb[i]<-0
  scenario_data$twinPits_rur[i]<-0
  scenario_data$onsiteDumpedland_urb[i]<-0.1
  scenario_data$onsiteDumpedland_rur[i]<-0.1
  
  if(!is.na(scenario_data$hdi[i])){
    if(scenario_data$hdi[i]<=hdi_cutoffs[1]){
      scenario_data$coverBury_urb[i]<-0
      scenario_data$coverBury_rur[i]<-pit_table_rur$med[1]+pit_table_rur$high[1]
      scenario_data$fecalSludgeTreated_urb[i]<-((pit_table_urb$med[1]+pit_table_urb$high[1])+(septic_table_urb$med[1]+septic_table_urb$high[1]))/2
      scenario_data$fecalSludgeTreated_rur[i]<-0
      scenario_data$isWatertight_urb[i]<-pit_table_urb$high[1]
      scenario_data$isWatertight_rur[i]<-pit_table_rur$high[1]
      scenario_data$hasLeach_urb[i]<-septic_table_urb$high[1]
      scenario_data$hasLeach_rur[i]<-septic_table_rur$med[1]+septic_table_rur$high[1]
    }else if(scenario_data$hdi[i]>hdi_cutoffs[1] & scenario_data$hdi[i]<=hdi_cutoffs[2]){
      scenario_data$coverBury_urb[i]<-0
      scenario_data$coverBury_rur[i]<-pit_table_rur$med[2]+pit_table_rur$high[2]
      scenario_data$fecalSludgeTreated_urb[i]<-((pit_table_urb$med[2]+pit_table_urb$high[2])+(septic_table_urb$med[2]+septic_table_urb$high[2]))/2
      scenario_data$fecalSludgeTreated_rur[i]<-0
      scenario_data$isWatertight_urb[i]<-pit_table_urb$high[2]
      scenario_data$isWatertight_rur[i]<-pit_table_rur$high[2]
      scenario_data$hasLeach_urb[i]<-septic_table_urb$high[2]
      scenario_data$hasLeach_rur[i]<-septic_table_rur$med[2]+septic_table_rur$high[2]
    }else if(scenario_data$hdi[i]>hdi_cutoffs[2] & scenario_data$hdi[i]<=hdi_cutoffs[3]){
      scenario_data$coverBury_urb[i]<-0
      scenario_data$coverBury_rur[i]<-pit_table_rur$med[3]+pit_table_rur$high[3]
      scenario_data$fecalSludgeTreated_urb[i]<-((pit_table_urb$med[3]+pit_table_urb$high[3])+(septic_table_urb$med[3]+septic_table_urb$high[3]))/2
      scenario_data$fecalSludgeTreated_rur[i]<-0
      scenario_data$isWatertight_urb[i]<-pit_table_urb$high[3]
      scenario_data$isWatertight_rur[i]<-pit_table_rur$high[3]
      scenario_data$hasLeach_urb[i]<-septic_table_urb$high[3]
      scenario_data$hasLeach_rur[i]<-septic_table_rur$med[3]+septic_table_rur$high[3]
    }else{ #hdi>hdi_cutoffs[3]
      scenario_data$coverBury_urb[i]<-0
      scenario_data$coverBury_rur[i]<-pit_table_rur$med[4]+pit_table_rur$high[4]
      scenario_data$fecalSludgeTreated_urb[i]<-((pit_table_urb$med[4]+pit_table_urb$high[4])+(septic_table_urb$med[4]+septic_table_urb$high[4]))/2
      scenario_data$fecalSludgeTreated_rur[i]<-0
      scenario_data$isWatertight_urb[i]<-pit_table_urb$high[4]
      scenario_data$isWatertight_rur[i]<-pit_table_rur$high[4]
      scenario_data$hasLeach_urb[i]<-septic_table_urb$high[4]
      scenario_data$hasLeach_rur[i]<-septic_table_rur$med[4]+septic_table_rur$high[4]
    }
  }
}
  

#calculate the treatment removal

volgorde$treatment<-NA


for(i in 1:length(reference_data$iso)){
  
  #first normalise Peter's data
  if(year==2010){
    scenario_data$FractionPrimarytreatment[i]<-data$prim$'2010'[volgorde$ints[i]]/(data$prim$'2010'[volgorde$ints[i]]+data$secu$'2010'[volgorde$ints[i]]+data$tert$'2010'[volgorde$ints[i]]+data$quat$'2010'[volgorde$ints[i]])
    scenario_data$FractionSecondarytreatment[i]<-data$secu$'2010'[volgorde$ints[i]]/(data$prim$'2010'[volgorde$ints[i]]+data$secu$'2010'[volgorde$ints[i]]+data$tert$'2010'[volgorde$ints[i]]+data$quat$'2010'[volgorde$ints[i]])
    scenario_data$FractionTertiarytreatment[i]<-data$tert$'2010'[volgorde$ints[i]]/(data$prim$'2010'[volgorde$ints[i]]+data$secu$'2010'[volgorde$ints[i]]+data$tert$'2010'[volgorde$ints[i]]+data$quat$'2010'[volgorde$ints[i]])
    #this is not ideal, but a quick fix: we use ponds for quaternary. This should be changed in the model! But currently anyway, only the fEmitted is used.
    scenario_data$FractionPonds[i]<-data$quat$'2010'[volgorde$ints[i]]/(data$prim$'2010'[volgorde$ints[i]]+data$secu$'2010'[volgorde$ints[i]]+data$tert$'2010'[volgorde$ints[i]]+data$quat$'2010'[volgorde$ints[i]])
    volgorde$treatment[i]<-(data$prim$'2010'[volgorde$ints[i]]+data$secu$'2010'[volgorde$ints[i]]+data$tert$'2010'[volgorde$ints[i]]+data$quat$'2010'[volgorde$ints[i]])/data$sewr$'2010'[volgorde$ints[i]]
  } else if(year==2020){
    scenario_data$FractionPrimarytreatment[i]<-data$prim$'2020'[volgorde$ints[i]]/(data$prim$'2020'[volgorde$ints[i]]+data$secu$'2020'[volgorde$ints[i]]+data$tert$'2020'[volgorde$ints[i]]+data$quat$'2020'[volgorde$ints[i]])
    scenario_data$FractionSecondarytreatment[i]<-data$secu$'2020'[volgorde$ints[i]]/(data$prim$'2020'[volgorde$ints[i]]+data$secu$'2020'[volgorde$ints[i]]+data$tert$'2020'[volgorde$ints[i]]+data$quat$'2020'[volgorde$ints[i]])
    scenario_data$FractionTertiarytreatment[i]<-data$tert$'2020'[volgorde$ints[i]]/(data$prim$'2020'[volgorde$ints[i]]+data$secu$'2020'[volgorde$ints[i]]+data$tert$'2020'[volgorde$ints[i]]+data$quat$'2020'[volgorde$ints[i]])
    #this is not ideal, but a quick fix: we use ponds for quaternary. This should be changed in the model! But currently anyway, only the fEmitted is used.
    scenario_data$FractionPonds[i]<-data$quat$'2020'[volgorde$ints[i]]/(data$prim$'2020'[volgorde$ints[i]]+data$secu$'2020'[volgorde$ints[i]]+data$tert$'2020'[volgorde$ints[i]]+data$quat$'2020'[volgorde$ints[i]])
    volgorde$treatment[i]<-(data$prim$'2020'[volgorde$ints[i]]+data$secu$'2020'[volgorde$ints[i]]+data$tert$'2020'[volgorde$ints[i]]+data$quat$'2020'[volgorde$ints[i]])/data$sewr$'2020'[volgorde$ints[i]]
  } else if (year==2030){
    scenario_data$FractionPrimarytreatment[i]<-data$prim$'2030'[volgorde$ints[i]]/(data$prim$'2030'[volgorde$ints[i]]+data$secu$'2030'[volgorde$ints[i]]+data$tert$'2030'[volgorde$ints[i]]+data$quat$'2030'[volgorde$ints[i]])
    scenario_data$FractionSecondarytreatment[i]<-data$secu$'2030'[volgorde$ints[i]]/(data$prim$'2030'[volgorde$ints[i]]+data$secu$'2030'[volgorde$ints[i]]+data$tert$'2030'[volgorde$ints[i]]+data$quat$'2030'[volgorde$ints[i]])
    scenario_data$FractionTertiarytreatment[i]<-data$tert$'2030'[volgorde$ints[i]]/(data$prim$'2030'[volgorde$ints[i]]+data$secu$'2030'[volgorde$ints[i]]+data$tert$'2030'[volgorde$ints[i]]+data$quat$'2030'[volgorde$ints[i]])
    #this is not ideal, but a quick fix: we use ponds for quaternary. This should be changed in the model! But currently anyway, only the fEmitted is used.
    scenario_data$FractionPonds[i]<-data$quat$'2030'[volgorde$ints[i]]/(data$prim$'2030'[volgorde$ints[i]]+data$secu$'2030'[volgorde$ints[i]]+data$tert$'2030'[volgorde$ints[i]]+data$quat$'2030'[volgorde$ints[i]])
    volgorde$treatment[i]<-(data$prim$'2030'[volgorde$ints[i]]+data$secu$'2030'[volgorde$ints[i]]+data$tert$'2030'[volgorde$ints[i]]+data$quat$'2030'[volgorde$ints[i]])/data$sewr$'2030'[volgorde$ints[i]]
  } else if (year==2050){
    scenario_data$FractionPrimarytreatment[i]<-data$prim$'2050'[volgorde$ints[i]]/(data$prim$'2050'[volgorde$ints[i]]+data$secu$'2050'[volgorde$ints[i]]+data$tert$'2050'[volgorde$ints[i]]+data$quat$'2050'[volgorde$ints[i]])
    scenario_data$FractionSecondarytreatment[i]<-data$secu$'2050'[volgorde$ints[i]]/(data$prim$'2050'[volgorde$ints[i]]+data$secu$'2050'[volgorde$ints[i]]+data$tert$'2050'[volgorde$ints[i]]+data$quat$'2050'[volgorde$ints[i]])
    scenario_data$FractionTertiarytreatment[i]<-data$tert$'2050'[volgorde$ints[i]]/(data$prim$'2050'[volgorde$ints[i]]+data$secu$'2050'[volgorde$ints[i]]+data$tert$'2050'[volgorde$ints[i]]+data$quat$'2050'[volgorde$ints[i]])
    #this is not ideal, but a quick fix: we use ponds for quaternary. This should be changed in the model! But currently anyway, only the fEmitted is used.
    scenario_data$FractionPonds[i]<-data$quat$'2050'[volgorde$ints[i]]/(data$prim$'2050'[volgorde$ints[i]]+data$secu$'2050'[volgorde$ints[i]]+data$tert$'2050'[volgorde$ints[i]]+data$quat$'2050'[volgorde$ints[i]])
    volgorde$treatment[i]<-(data$prim$'2050'[volgorde$ints[i]]+data$secu$'2050'[volgorde$ints[i]]+data$tert$'2050'[volgorde$ints[i]]+data$quat$'2050'[volgorde$ints[i]])/data$sewr$'2050'[volgorde$ints[i]]
  } else if (year==2070){
    scenario_data$FractionPrimarytreatment[i]<-data$prim$'2070'[volgorde$ints[i]]/(data$prim$'2070'[volgorde$ints[i]]+data$secu$'2070'[volgorde$ints[i]]+data$tert$'2070'[volgorde$ints[i]]+data$quat$'2070'[volgorde$ints[i]])
    scenario_data$FractionSecondarytreatment[i]<-data$secu$'2070'[volgorde$ints[i]]/(data$prim$'2070'[volgorde$ints[i]]+data$secu$'2070'[volgorde$ints[i]]+data$tert$'2070'[volgorde$ints[i]]+data$quat$'2070'[volgorde$ints[i]])
    scenario_data$FractionTertiarytreatment[i]<-data$tert$'2070'[volgorde$ints[i]]/(data$prim$'2070'[volgorde$ints[i]]+data$secu$'2070'[volgorde$ints[i]]+data$tert$'2070'[volgorde$ints[i]]+data$quat$'2070'[volgorde$ints[i]])
    #this is not ideal, but a quick fix: we use ponds for quaternary. This should be changed in the model! But currently anyway, only the fEmitted is used.
    scenario_data$FractionPonds[i]<-data$quat$'2070'[volgorde$ints[i]]/(data$prim$'2070'[volgorde$ints[i]]+data$secu$'2070'[volgorde$ints[i]]+data$tert$'2070'[volgorde$ints[i]]+data$quat$'2070'[volgorde$ints[i]])
    volgorde$treatment[i]<-(data$prim$'2070'[volgorde$ints[i]]+data$secu$'2070'[volgorde$ints[i]]+data$tert$'2070'[volgorde$ints[i]]+data$quat$'2070'[volgorde$ints[i]])/data$sewr$'2070'[volgorde$ints[i]]
  }

#calculate the fraction sewage treated
  
  #all treatment goes to urban, unless urban is smaller than total treatment, then the remainder goes to rural
  pop_sewer_urb<-scenario_data$population[i]*scenario_data$fraction_urban_pop[i]*scenario_data$flushSewer_urb[i]
  pop_sewer_rur<-scenario_data$population[i]*(1-scenario_data$fraction_urban_pop[i])*scenario_data$flushSewer_rur[i]
  pop_sewer_treated<-(pop_sewer_urb+pop_sewer_rur)*volgorde$treatment[i]
  if (!is.na(pop_sewer_treated)){
    if (pop_sewer_urb<pop_sewer_treated){
      scenario_data$sewageTreated_urb[i]<-1
      pop_treated_rur<-pop_sewer_treated-pop_sewer_rur
      scenario_data$sewageTreated_rur[i]<-(pop_sewer_treated-pop_sewer_urb)/pop_sewer_rur
    } else if (pop_sewer_urb==pop_sewer_treated){
      scenario_data$sewageTreated_urb[i]<-1
      scenario_data$sewageTreated_rur[i]<-0
    } else if (pop_sewer_urb>pop_sewer_treated){
      scenario_data$sewageTreated_urb[i]<-pop_sewer_treated/pop_sewer_urb
      scenario_data$sewageTreated_rur[i]<-0
    }
  }
}

#then calculate the removal, using data from the sketcher

#Category	  Description	        Group	  Log10 Reduction	Percent Reduction	Percent in Liquid Effluent	Percent in Sludge/ Biosolids
#Category 1	Primary Treatment	  Viruses	  0.6	75%	97%	3%
#Category 2	Secondary Treatment	Viruses	  1.3	95%	50%	50%
#Category 3	Tertiary Treatment	Viruses	  2.0	99%	40%	60%
#Category 4 Quaternary Treatment Viruses  3.0    
#Category 1	Primary Treatment	  Bacteria	0.6	75%	99%	1%
#Category 2	Secondary Treatment	Bacteria	2.0	99%	95%	5%
#Category 3	Tertiary Treatment	Bacteria	2.3	99.5%	95%	5%
#Category 1	Primary Treatment	  Protozoa	0.3	50%	85%	15%
#Category 2	Secondary Treatment	Protozoa	1.0	90%	20%	80%
#Category 3	Tertiary Treatment	Protozoa	1.1	92%	25%	75%
#Category 1	Primary Treatment	  Helminth	1.3	95%	20%	80%
#Category 2	Secondary Treatment	Helminth	1.4	96%	1%	99%
#Category 3	Tertiary Treatment	Helminth	1.5	97%	1%	99%

primary_viruses<-0.75
primary_viruses_liquid<-0.97
secondary_viruses<-0.95
secondary_viruses_liquid<-0.50
tertiary_viruses<-0.99
tertiary_viruses_liquid<-0.40
quaternary_viruses<-0.9993  #for quaternary, I added ammonium disinfection and composting to the tertiary treatment
quaternary_viruses_liquid<-0.05
    
primary_protozoa<-0.5
primary_protozoa_liquid<-0.85
secondary_protozoa<-0.90
secondary_protozoa_liquid<-0.20
tertiary_protozoa<-0.92
tertiary_protozoa_liquid<-0.25
quaternary_protozoa<-0.994
quaternary_protozoa_liquid<-0.03

primary_viruses_femittedaftertreatment<-primary_viruses_liquid-primary_viruses_liquid*primary_viruses
secondary_viruses_femittedaftertreatment<-secondary_viruses_liquid-secondary_viruses_liquid*secondary_viruses
tertiary_viruses_femittedaftertreatment<-tertiary_viruses_liquid-tertiary_viruses_liquid*tertiary_viruses
quaternary_viruses_femittedaftertreatment<-quaternary_viruses_liquid-quaternary_viruses_liquid*quaternary_viruses

primary_protozoa_femittedaftertreatment<-primary_protozoa_liquid-primary_protozoa_liquid*primary_protozoa
secondary_protozoa_femittedaftertreatment<-secondary_protozoa_liquid-secondary_protozoa_liquid*secondary_protozoa
tertiary_protozoa_femittedaftertreatment<-tertiary_protozoa_liquid-tertiary_protozoa_liquid*tertiary_protozoa
quaternary_protozoa_femittedaftertreatment<-quaternary_protozoa_liquid-quaternary_protozoa_liquid*quaternary_protozoa

scenario_data$fRemoval_treatment_virus<-scenario_data$FractionPrimarytreatment*primary_viruses+scenario_data$FractionSecondarytreatment*secondary_viruses+scenario_data$FractionTertiarytreatment*tertiary_viruses+scenario_data$FractionPonds*quaternary_viruses
scenario_data$fRemoval_treatment_protozoa<-scenario_data$FractionPrimarytreatment*primary_protozoa+scenario_data$FractionSecondarytreatment*secondary_protozoa+scenario_data$FractionTertiarytreatment*tertiary_protozoa++scenario_data$FractionPonds*quaternary_protozoa

scenario_data$fEmitted_inEffluent_after_treatment_virus<-scenario_data$FractionPrimarytreatment*primary_viruses_femittedaftertreatment+scenario_data$FractionSecondarytreatment*secondary_viruses_femittedaftertreatment+scenario_data$FractionTertiarytreatment*tertiary_viruses_femittedaftertreatment+scenario_data$FractionPonds*quaternary_viruses_femittedaftertreatment
scenario_data$fEmitted_inEffluent_after_treatment_protozoa<-scenario_data$FractionPrimarytreatment*primary_protozoa_femittedaftertreatment+scenario_data$FractionSecondarytreatment*secondary_protozoa_femittedaftertreatment+scenario_data$FractionTertiarytreatment*tertiary_protozoa_femittedaftertreatment+scenario_data$FractionPonds*quaternary_protozoa_femittedaftertreatment

#scenario_data$efficiency<-scenario_data$population*scenario_data$fraction_urban_pop*scenario_data$sewageTreated_urb*(1-scenario_data$fEmitted_inEffluent_after_treatment_protozoa)+scenario_data$population*(1-scenario_data$fraction_urban_pop)*scenario_data$sewageTreated_rur*(1-scenario_data$fEmitted_inEffluent_after_treatment_protozoa)
#scen_data_tot<-sum(scenario_data$efficiency,na.rm=TRUE)/sum(scenario_data$population,na.rm=TRUE)

#print(c(year, scenario, scen_data_tot))

#save scenario file
 
write.csv(scenario_data,file=paste0(working.path.in,"Input_file_world_country_",scenario,"_",year,".csv"))

}
}
