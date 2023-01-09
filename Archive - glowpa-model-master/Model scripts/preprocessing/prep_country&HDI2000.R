# Preprocessing script for 2000 country grid and HDI grid


########################### Fixing states US and China and making HDI on 30min grid ####


pointdatafile<-paste(working.path.in,"MA_AM_AD.TXT",sep="")
data_point<-read.csv(pointdatafile,sep="",header=TRUE,col.names=c("cnt", "region", "country", "pop_num1970", "pop_num1990", "pop_num2000", "pop_num2030", "pop_num2050", "pop_urb1970", "pop_urb1990", "pop_urb2000", "pop_urb2030", "pop_urb2050", "pop_gdp1970", "pop_gdp1990", "pop_gdp2000", "pop_gdp2030", "pop_gdp2050", "san_urb1970", "san_urb1990", "san_urb2000", "san_urb2030", "san_urb2050", "san_rur1970", "san_rur1990", "san_rur2000", "san_rur2030", "san_rur2050", "human_N1970", "human_N1990", "human_N2000", "human_N2030", "human_N2050", "human_P1970", "human_P1990", "human_P2000", "human_P2030", "human_P2050", "used_c", "pop_con1970", "pop_con1990", "pop_con2000", "pop_con2030", "pop_con2050", "pop_urb_con1970", "pop_urb_con1990", "pop_urb_con2000", "pop_urb_con2030", "pop_urb_con2050", "pop_rur_con1970", "pop_rur_con1990", "pop_rur_con2000", "pop_rur_con2030", "pop_rur_con2050", "treatm_1_1970", "treatm_1_1990", "treatm_1_2000", "treatm_1_2030", "treatm_1_2050", "treatm_2_1970", "treatm_2_1990", "treatm_2_2000", "treatm_2_2030", "treatm_2_2050", "treatm_3_1970", "treatm_3_1990", "treatm_3_2000", "treatm_3_2030", "treatm_3_2050", "N_removal_T1", "N_removal_T2", "N_removal_T3", "P_removal_T1", "P_removal_T2", "P_removal_T3", "N_retention1970", "N_retention1990", "N_retention2000", "N_retention2030", "N_retention2050", "P_retention1970", "P_retention1990", "P_retention2000", "P_retention2030", "P_retention2050", "mer_ppp1970", "mer_ppp1990", "mer_ppp2000", "mer_ppp2030", "mer_ppp2050", "dishwsh_coverage1970", "dishwsh_coverage1990", "dishwsh_coverage2000", "dishwsh_coverage2030", "dishwsh_coverage2050", "dishwsh_det_cons1970", "dishwsh_det_cons1990", "dishwsh_det_cons2000", "dishwsh_det_cons2030", "dishwsh_det_cons2050", "dishwsh_det_P_cons1970", "dishwsh_det_P_cons1990", "dishwsh_det_P_cons2000", "dishwsh_det_P_cons2030", "dishwsh_det_P_cons2050", "laundry_det_cons1970", "laundry_det_cons1990", "laundry_det_cons2000", "laundry_det_cons2030", "laundry_det_cons2050", "laundry_det_P_cons1970", "laundry_det_P_cons1990", "laundry_det_P_cons2000", "laundry_det_P_cons2030", "laundry_det_P_cons2050", "laundry_det_P_free1970", "laundry_det_P_free1990", "laundry_det_P_free2000", "laundry_det_P_free2030", "laundry_det_P_free2050"))

#Read HDI (Human Development Index) data
hdifile<-paste(working.path.in,"HDI.csv",sep="")
data_hdi<-read.csv(hdifile,sep=",",header=TRUE,col.names=c("iso","hdi"))
a<-which(data_hdi$hdi == -9999)
data_hdi$hdi[a]<-NA
d<-0

for (i in 1:length(data_point[,1])){
  a<-which(data_hdi$iso[] == data_point$cnt[i])
  if(length(a) > 0){d<-d+1}
}


data_table<-array(dim=c(d,7))
colnames(data_table)<-c("iso","population","connected_population","primary_treatment","secondary_treatment","tertiary_treatment","hdi")
b<-0



for (i in 1:length(data_point[,1])){
  a<-which(data_hdi$iso[] == data_point$cnt[i])
  if (length(a) == 0){b<-b+1}else{
    data_table[i-b,1]<-data_point$cnt[i]
    data_table[i-b,3]<-data_point$pop_con2000[i]
    data_table[i-b,4]<-data_point$treatm_1_2000[i]
    data_table[i-b,5]<-data_point$treatm_2_2000[i]
    data_table[i-b,6]<-data_point$treatm_3_2000[i]
    data_table[i-b,7]<-data_hdi$hdi[a]
  }
}


data_table1<-as.data.frame(data_table)

garealandfile<-paste(working.path.in,"GAREALAND.map",sep="")
hdi_grid<-(readAsciiGrid(garealandfile))

#Read population & country data at 30min grid
#wordt dit gebruikt?
pop30minfile<-paste(working.path.in,"pop30min.asc",sep="")
population_grids<-(readAsciiGrid(pop30minfile))
landenfile<-paste(working.path.in,"Landen.map",sep="")
countries<-(readAsciiGrid(landenfile))
#States of US and China are all given US country number 840 or China country number 156
a<-which((countries$Landen.map > 1500) & (countries$Landen.map < 1600))
countries$Landen.map[a]<-156
a<-which(countries$Landen.map > 3900 & countries$Landen.map < 4100)
countries$Landen.map[a]<-840



#Put HDI in correct gridfile
hdi_grid[[1]]<-NA
for (i in 1:length(data_table1$hdi)){
  a<-which(countries$Landen.map == data_table1$iso[i])
  if (length(a) == 0 || is.na(data_table1$hdi[i])){b<-b+1}else{
    (hdi_grid$GAREALAND.map[a]<- data_table1$hdi[i])
  }
}

#Write HDI grid
fname<-paste(working.path.in,"HDIgrid30min.asc",sep="")
writeAsciiGrid(hdi_grid, fname, attr = 1, na.value = -9999)

#Write country grid with states fixed
fname<-paste(working.path.in,"Countries30minNoStates.asc",sep="")
writeAsciiGrid(countries, fname, attr = 1, na.value = -9999)

####
# Make again a new HDI gridded file, now based on a file with the codes 901-903 fixed

Countries30minNoStates <- (readAsciiGrid(paste(working.path.in,"Countries30minNoStates.asc",sep="")))   #has ISO codes going to 904, leading to holes in some parts of Europe, needs fixing. HDI files made based on this file also need fixing
#Fix some numbers that appear on this map erroneously
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 901)  #Normandy
Countries30minNoStates$Countries30minNoStates.asc[a]<-250           #France
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 902)  #Po Plain
Countries30minNoStates$Countries30minNoStates.asc[a]<-380           #Italy
a<-which(Countries30minNoStates$Countries30minNoStates.asc == 903)  #north Spain
Countries30minNoStates$Countries30minNoStates.asc[a]<-724           #Spain

#Put HDI in correct gridfile
hdi_grid[[1]]<-NA
b<-0
for (i in 1:length(data_table1$hdi)){
  a<-which(Countries30minNoStates$Countries30minNoStates.asc == data_table1$iso[i])
  if (length(a) == 0 || is.na(data_table1$hdi[i])){b<-b+1}else{
    (hdi_grid$GAREALAND.map[a]<- data_table1$hdi[i])
  }
}


hdiraster<-raster(hdi_grid)
plot(hdiraster)

check<-raster(Countries30minNoStates)
plot(check)

#Write HDI grid
fname<-paste(working.path.in,"HDIgrid30min.asc",sep="")
writeAsciiGrid(hdi_grid, fname, attr = 1, na.value = -9999)

