#population 5 min grid summing


##################### summing population from 5 min grid to country #####

#Read population data at 5 min grid and convert to country data. This to make sure there is no difference later on in calculations
#Uitzoeken waar precies welke populatie wordt gebruikt en waarom
#If a country consists of multiple grids, population values of those grids are summed
# dit eruit halen en onder pre-processing stoppen
# append aan de CountryDataPoint file
CountryDataPoint<-read.csv(paste(working.path.in,"MA_AM_AD.TXT",sep=""),sep="",header=TRUE,col.names=c("cnt", "region", "country", "pop_num1970", "pop_num1990", "pop_num2000", "pop_num2030", "pop_num2050", "pop_urb1970", "pop_urb1990", "pop_urb2000", "pop_urb2030", "pop_urb2050", "pop_gdp1970", "pop_gdp1990", "pop_gdp2000", "pop_gdp2030", "pop_gdp2050", "san_urb1970", "san_urb1990", "san_urb2000", "san_urb2030", "san_urb2050", "san_rur1970", "san_rur1990", "san_rur2000", "san_rur2030", "san_rur2050", "human_N1970", "human_N1990", "human_N2000", "human_N2030", "human_N2050", "human_P1970", "human_P1990", "human_P2000", "human_P2030", "human_P2050", "used_c", "pop_con1970", "pop_con1990", "pop_con2000", "pop_con2030", "pop_con2050", "pop_urb_con1970", "pop_urb_con1990", "pop_urb_con2000", "pop_urb_con2030", "pop_urb_con2050", "pop_rur_con1970", "pop_rur_con1990", "pop_rur_con2000", "pop_rur_con2030", "pop_rur_con2050", "treatm_1_1970", "treatm_1_1990", "treatm_1_2000", "treatm_1_2030", "treatm_1_2050", "treatm_2_1970", "treatm_2_1990", "treatm_2_2000", "treatm_2_2030", "treatm_2_2050", "treatm_3_1970", "treatm_3_1990", "treatm_3_2000", "treatm_3_2030", "treatm_3_2050", "N_removal_T1", "N_removal_T2", "N_removal_T3", "P_removal_T1", "P_removal_T2", "P_removal_T3", "N_retention1970", "N_retention1990", "N_retention2000", "N_retention2030", "N_retention2050", "P_retention1970", "P_retention1990", "P_retention2000", "P_retention2030", "P_retention2050", "mer_ppp1970", "mer_ppp1990", "mer_ppp2000", "mer_ppp2030", "mer_ppp2050", "dishwsh_coverage1970", "dishwsh_coverage1990", "dishwsh_coverage2000", "dishwsh_coverage2030", "dishwsh_coverage2050", "dishwsh_det_cons1970", "dishwsh_det_cons1990", "dishwsh_det_cons2000", "dishwsh_det_cons2030", "dishwsh_det_cons2050", "dishwsh_det_P_cons1970", "dishwsh_det_P_cons1990", "dishwsh_det_P_cons2000", "dishwsh_det_P_cons2030", "dishwsh_det_P_cons2050", "laundry_det_cons1970", "laundry_det_cons1990", "laundry_det_cons2000", "laundry_det_cons2030", "laundry_det_cons2050", "laundry_det_P_cons1970", "laundry_det_P_cons1990", "laundry_det_P_cons2000", "laundry_det_P_cons2030", "laundry_det_P_cons2050", "laundry_det_P_free1970", "laundry_det_P_free1990", "laundry_det_P_free2000", "laundry_det_P_free2030", "laundry_det_P_free2050"))
Countries5minNoStates <- (readAsciiGrid(paste(working.path.in,"Countries5minNoStates.asc",sep="")))
Population5min <- (readAsciiGrid(paste(working.path.in,"POP_2000.asc",sep="")))  

CountryPopSummedFrom5min<-array(dim=(length(CountryDataPoint$cnt)))
for (i in 1:length(CountryDataPoint$cnt)){
  a<-which(Countries5minNoStates$Countries5minNoStates.asc==CountryDataPoint$cnt[i]) 
  if (length(a)>0){
    CountryPopSummedFrom5min[i]<-sum(Population5min$POP_2000.asc[a],na.rm=TRUE)
  } else CountryPopSummedFrom5min[i]<-NA
}
rm(a)
write.csv(CountryPopSummedFrom5min, file="CountryPopSummedFrom5min.csv")

# this information was then added as extra column to the MA_AM_AD_Lucie.txt file
