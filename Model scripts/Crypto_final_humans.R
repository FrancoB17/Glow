# Cryptosporidium human emissions model


#add column to calculate overall removal fractions
CountryDataPoint$removalfraction<- NA
#add columns to calculate urban and rural population numbers
CountryDataPoint$pop_urb_num<-NA
CountryDataPoint$pop_rur_num<-NA
#add columns to calculate population number of children under 5 in urban and rural areas
CountryDataPoint$pop_urb_under5<-NA
CountryDataPoint$pop_rur_under5<-NA
#add columns to calculate population number of children under 5-14 in urban and rural areas
CountryDataPoint$pop_urb_5to14<-NA
CountryDataPoint$pop_rur_5to14<-NA
#add columns to calculate population number of children under 15-24 in urban and rural areas
CountryDataPoint$pop_urb_15to24<-NA
CountryDataPoint$pop_rur_15to24<-NA
#add columns to calculate population number of children under 25plus in urban and rural areas
CountryDataPoint$pop_urb_25plus<-NA
CountryDataPoint$pop_rur_25plus<-NA


#fill all the added columns
for (i in 1:length(CountryDataPoint$iso)){
  CountryDataPoint$removalfraction[i]<- 1-(CountryDataPoint$treatm_1[i]*cryptoRemovalPrimary+CountryDataPoint$treatm_2[i]*cryptoRemovalSecondary+CountryDataPoint$treatm_3[i]*cryptoRemovalTertiary)
  CountryDataPoint$pop_urb_num[i]<-CountryDataPoint$pop_totalx1000[i]*1000*CountryDataPoint$f_urb[i]
  CountryDataPoint$pop_rur_num[i]<-CountryDataPoint$pop_totalx1000[i]*1000*CountryDataPoint$f_rur[i]
  CountryDataPoint$pop_urb_under5[i]<- CountryDataPoint$pop_urb_num[i]*CountryDataPoint$f_under5[i]
  CountryDataPoint$pop_rur_under5[i]<- CountryDataPoint$pop_rur_num[i]*CountryDataPoint$f_under5[i]
  CountryDataPoint$pop_urb_5to14[i]<- CountryDataPoint$pop_urb_num[i]*CountryDataPoint$f_5to14[i]
  CountryDataPoint$pop_rur_5to14[i]<- CountryDataPoint$pop_rur_num[i]*CountryDataPoint$f_5to14[i]
  CountryDataPoint$pop_urb_15to24[i]<- CountryDataPoint$pop_urb_num[i]*CountryDataPoint$f_15to24[i]
  CountryDataPoint$pop_rur_15to24[i]<- CountryDataPoint$pop_rur_num[i]*CountryDataPoint$f_15to24[i]
  CountryDataPoint$pop_urb_25plus[i]<- CountryDataPoint$pop_urb_num[i]*CountryDataPoint$f_25plus[i]
  CountryDataPoint$pop_rur_25plus[i]<- CountryDataPoint$pop_rur_num[i]*CountryDataPoint$f_25plus[i]
}
rm(i)


#make dataframe to fill with urban and rural Cryptosporidium emissions
k<-length(CountryDataPoint$iso)
point<-array(dim=c(k,24))
colnames(point)<-c("countryname","iso","crypto_urb","crypto_rur","crypto_urb_pp","crypto_rur_pp","crypto_urb_dif_pp","crypto_rur_dif_pp","crypto_urb_under5","crypto_rur_under5","crypto_urb_others","crypto_rur_others","crypto_urb_pp_under5","crypto_rur_pp_under5","crypto_urb_pp_others","crypto_rur_pp_others","crypto_urb_con","crypto_rur_con","crypto_urb_dir","crypto_rur_dir","crypto_urb_dif","crypto_rur_dif","crypto_urb_non","crypto_rur_non")
rm(k)
point<-as.data.frame(point)

#find developed and developing countries & identify excretion categories
#a = developing, b=developed
#c1=africa, c2=asia, c3=rest of the world
#identify countries that fall into the different excretion categories
ac2<-which(CountryDataPoint$hdi <= HDIboundary) #low HDI
bc2<-which(CountryDataPoint$hdi > HDIboundary)  #high HDI
       

#For <5:
#bc2 en bc3 moeten alles 1/2 nappy correction krijgen
#ac1, ac2 en ac3 moeten alleen nappy correction krijgen voor sewage

#fill dataframe
point$countryname<-CountryDataPoint$countryname
point$iso<-CountryDataPoint$iso

#for HDI high
for (i in 1:length(CountryDataPoint$iso[bc2])){
#  print(i)
point$crypto_urb_under5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_urb_under5[bc2[i]]*
                              ( CountryDataPoint$con_urb[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]]+  #urban connected
                                CountryDataPoint$dir_urb[bc2[i]]*UrbanDirect2Water+                         #urban direct
                                #CountryDataPoint$dif_urb[bc2[i]]*UrbanDiffuse2Water+                        #urban diffuse
                                CountryDataPoint$non_urb[bc2[i]]*UrbanNonSource2Water))  #urban nonsource

#print(i)
  
point$crypto_rur_under5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_rur_under5[bc2[i]]*
                              ( CountryDataPoint$con_rur[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]]+  #rural connected
                                CountryDataPoint$dir_rur[bc2[i]]*RuralDirect2Water+                         #rural direct
                                #CountryDataPoint$dif_rur[bc2[i]]*RuralDiffuse2Water+                        #rural diffuse
                                CountryDataPoint$non_rur[bc2[i]]*RuralNonSource2Water))   #rural nonsource                                                            

#print(i)

point$crypto_urb_others[bc2[i]]<-cryptoExcretionHighHDI_others*(CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])*
  ( CountryDataPoint$con_urb[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]]+  #urban connected
      CountryDataPoint$dir_urb[bc2[i]]*UrbanDirect2Water+                         #urban direct
      #CountryDataPoint$dif_urb[bc2[i]]*UrbanDiffuse2Water+                        #urban diffuse
      CountryDataPoint$non_urb[bc2[i]]*UrbanNonSource2Water)                     #urban nonsource

#print(i)

point$crypto_rur_others[bc2[i]]<-cryptoExcretionHighHDI_others*(CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])*
  ( CountryDataPoint$con_rur[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]]+  #rural connected
      CountryDataPoint$dir_rur[bc2[i]]*RuralDirect2Water+                         #rural direct
      #CountryDataPoint$dif_rur[bc2[i]]*RuralDiffuse2Water+                        #rural diffuse
      CountryDataPoint$non_rur[bc2[i]]*RuralNonSource2Water)  

point$crypto_urb_con[bc2[i]]<-CountryDataPoint$con_urb[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]]*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_urb_under5[bc2[i]]+CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])
point$crypto_rur_con[bc2[i]]<-CountryDataPoint$con_rur[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]]*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_rur_under5[bc2[i]]+CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])
point$crypto_urb_dir[bc2[i]]<-CountryDataPoint$dir_urb[bc2[i]]*UrbanDirect2Water*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_urb_under5[bc2[i]]+CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])
point$crypto_rur_dir[bc2[i]]<-CountryDataPoint$dir_rur[bc2[i]]*RuralDirect2Water*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_rur_under5[bc2[i]]+CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])
point$crypto_urb_dif[bc2[i]]<-CountryDataPoint$dif_urb[bc2[i]]*UrbanDiffuse2Water*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_urb_under5[bc2[i]]+CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])
point$crypto_rur_dif[bc2[i]]<-CountryDataPoint$dif_rur[bc2[i]]*RuralDiffuse2Water*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_rur_under5[bc2[i]]+CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])
point$crypto_urb_non[bc2[i]]<-CountryDataPoint$non_urb[bc2[i]]*UrbanNonSource2Water*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_urb_under5[bc2[i]]+CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])
point$crypto_rur_non[bc2[i]]<-CountryDataPoint$non_rur[bc2[i]]*UrbanNonSource2Water*cryptoExcretionHighHDI_others*(NappyCor*CountryDataPoint$pop_rur_under5[bc2[i]]+CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])

}
rm(i)

#for HDI low
for (i in 1:length(CountryDataPoint$iso[ac2])){
  point$crypto_urb_under5[ac2[i]]<-cryptoExcretionLowHDI_child*CountryDataPoint$pop_urb_under5[ac2[i]]*
                                             ( CountryDataPoint$con_urb[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]]*NappyCor+  #urban connected
                                                 CountryDataPoint$dir_urb[ac2[i]]*UrbanDirect2Water+                         #urban direct
                                                 #CountryDataPoint$dif_urb[ac2[i]]*UrbanDiffuse2Water+                        #urban diffuse
                                                 CountryDataPoint$non_urb[ac2[i]]*UrbanNonSource2Water)                     #urban nonsource
  
  point$crypto_rur_under5[ac2[i]]<-cryptoExcretionLowHDI_child*CountryDataPoint$pop_rur_under5[ac2[i]]*
                                             ( CountryDataPoint$con_rur[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]]*NappyCor+  #rural connected
                                                 CountryDataPoint$dir_rur[ac2[i]]*RuralDirect2Water+                         #rural direct
                                                 #CountryDataPoint$dif_rur[ac2[i]]*RuralDiffuse2Water+                        #rural diffuse
                                                 CountryDataPoint$non_rur[ac2[i]]*RuralNonSource2Water)   #rural nonsource                                                            
  
  point$crypto_urb_others[ac2[i]]<-cryptoExcretionLowHDI_others*(CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])*
    ( CountryDataPoint$con_urb[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]]+  #urban connected
        CountryDataPoint$dir_urb[ac2[i]]*UrbanDirect2Water+                         #urban direct
        #CountryDataPoint$dif_urb[ac2[i]]*UrbanDiffuse2Water+                        #urban diffuse
        CountryDataPoint$non_urb[ac2[i]]*UrbanNonSource2Water)                     #urban nonsource
  
  point$crypto_rur_others[ac2[i]]<-cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])*
  ( CountryDataPoint$con_rur[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]]+  #rural connected
      CountryDataPoint$dir_rur[ac2[i]]*RuralDirect2Water+                         #rural direct
      #CountryDataPoint$dif_rur[ac2[i]]*RuralDiffuse2Water+                        #rural diffuse
      CountryDataPoint$non_rur[ac2[i]]*RuralNonSource2Water)  
  
  point$crypto_urb_con[ac2[i]]<-CountryDataPoint$con_urb[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]]*cryptoExcretionLowHDI_others*(NappyCor*CountryDataPoint$pop_urb_under5[ac2[i]]+CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])
  point$crypto_rur_con[ac2[i]]<-CountryDataPoint$con_rur[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]]*cryptoExcretionLowHDI_others*(NappyCor*CountryDataPoint$pop_rur_under5[ac2[i]]+CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])
  point$crypto_urb_dir[ac2[i]]<-CountryDataPoint$dir_urb[ac2[i]]*UrbanDirect2Water*cryptoExcretionLowHDI_others*(CountryDataPoint$pop_urb_under5[ac2[i]]+CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])
  point$crypto_rur_dir[ac2[i]]<-CountryDataPoint$dir_rur[ac2[i]]*RuralDirect2Water*cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_under5[ac2[i]]+CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])
  point$crypto_urb_dif[ac2[i]]<-CountryDataPoint$dif_urb[ac2[i]]*UrbanDiffuse2Water*cryptoExcretionLowHDI_others*(CountryDataPoint$pop_urb_under5[ac2[i]]+CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])
  point$crypto_rur_dif[ac2[i]]<-CountryDataPoint$dif_rur[ac2[i]]*RuralDiffuse2Water*cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_under5[ac2[i]]+CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])
  point$crypto_urb_non[ac2[i]]<-CountryDataPoint$non_urb[ac2[i]]*UrbanNonSource2Water*cryptoExcretionLowHDI_others*(CountryDataPoint$pop_urb_under5[ac2[i]]+CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])
  point$crypto_rur_non[ac2[i]]<-CountryDataPoint$non_rur[ac2[i]]*UrbanNonSource2Water*cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_under5[ac2[i]]+CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])
}
rm(i)

      
#Calculating average emission per head of the population (total population, not just under 5s, needed for compatibility with density ranking later)
for (i in 1:length(CountryDataPoint$iso)){
      point$crypto_urb[i]<-point$crypto_urb_under5[i]+point$crypto_urb_others[i]
      point$crypto_rur[i]<-point$crypto_rur_under5[i]+point$crypto_rur_others[i]
  
      point$crypto_urb_pp[i]<-point$crypto_urb[i]/CountryDataPoint$pop_urb_num[i]
      point$crypto_rur_pp[i]<-point$crypto_rur[i]/CountryDataPoint$pop_rur_num[i]
      
      point$crypto_urb_pp_under5[i]<-point$crypto_urb_under5[i]/CountryDataPoint$pop_urb_num[i] #this is not the real emission per person per age category, just a help to calculate the values for the map
      point$crypto_rur_pp_under5[i]<-point$crypto_rur_under5[i]/CountryDataPoint$pop_rur_num[i]
      
      point$crypto_urb_pp_others[i]<-point$crypto_urb_others[i]/CountryDataPoint$pop_urb_num[i] #this is not the real emission per person per age category, just a help to calculate the values for the map
      point$crypto_rur_pp_others[i]<-point$crypto_rur_others[i]/CountryDataPoint$pop_rur_num[i]
      
      point$crypto_urb_dif_pp[i]<-point$crypto_urb_dif[i]/CountryDataPoint$pop_urb_num[i] #this is just a help to calculate the values for the map
      point$crypto_rur_dif_pp[i]<-point$crypto_rur_dif[i]/CountryDataPoint$pop_rur_num[i]
      
      
      
} 
rm(i)

write.csv(point, file=paste0(working.path.out,"HumanEmissionsCalculated",scenario,".csv"))


############# Spatial distribution ########################################################################
#new density ranking approach
#we identify 'urban' and 'rural'population grid cells based on density
# every country will have one grid cell that has both urban and rural population
#we then assume an average urban excretion rate per person and an average rural excretion rate per person
# this way the emissions are no longer just concentrated in the few most densely populated cells,
# but instead spread out over the entire country population

garealandfile<-paste(working.path.in,"GAREALAND.map",sep="")
popurban_grid<-(readAsciiGrid(garealandfile)) #urban population grid, main output 
urban_grid<-(readAsciiGrid(garealandfile)) #grids die al geweest zijn en dus alle mensen daar wel urban 1 = geweest, 2 = deels geweest
poprural_grid<-(readAsciiGrid(garealandfile)) #rural population grid
population_km2<-(readAsciiGrid(garealandfile)) 
crypto_grid<-(readAsciiGrid(garealandfile)) #emissies populatie niet connected
crypto_grid_under5<-(readAsciiGrid(garealandfile))
crypto_grid_others<-(readAsciiGrid(garealandfile))
crypto_grid_diffuse<-(readAsciiGrid(garealandfile)) #added, this one should then go the the diffuse runoff script


population_km2[[1]]<-Population30min[[1]]/LandArea[[1]]  
popurban_grid[[1]]<-NA
urban_grid[[1]]<-NA
poprural_grid[[1]]<-NA
crypto_grid[[1]]<-NA
crypto_grid_under5[[1]]<-NA
crypto_grid_others[[1]]<-NA
crypto_grid_diffuse[[1]]<-NA


for (i in 1:length(CountryDataPoint$iso)){
  #selecteer gridcellen van een land
  a<-which(Countries30minNoStates$Countries30minNoStates.asc==CountryDataPoint$iso[i])
  #verhoog landscan grids met populatiegroei
  if(CountryDataPoint$iso[i]==population2010$iso[i]){
    dif<-CountryDataPoint$pop_totalx1000[i]/population2010$pop_totalx1000[i]
    Population30min$landscan2010.asc[a]<-Population30min$landscan2010.asc[a]*dif
  } else print("error1")
  #maak tabel met lengte aantal gridcellen van een land
  z<-array(dim=length(a))
  #bepaal negatieve populatie per km2 per cel
  pop<--population_km2$GAREALAND.map[a]
  #rank deze cellen, methode min betekent dat dichtstbevolkte de hoogste rank krijgen
  z<-rank(pop,ties.method="min")
  # b is je rank, begin bij 1
  b<-1
  sumgrid<-0
  #wij verdelen CountryDataPoint$pop_urb_num[i]
  if (is.na(CountryDataPoint$pop_urb_num[i])){
    popurban_grid$GAREALAND.map[a]<-NA
  } else {
    if(CountryDataPoint$pop_urb_num[i]>0){
      for (j in 1:length(a)){
        if (sumgrid<CountryDataPoint$pop_urb_num[i]){
          q<-which(z==b)
          
          if (length(q)>0){
            if(is.na(population_km2$GAREALAND.map[a[q[1]]])){
              b<-b+1
            } else {
              if ((CountryDataPoint$pop_urb_num[i]-(sumgrid+(Population30min$landscan2010.asc[a[q[1]]]*length(q)))) >= 0){ 
                popurban_grid$GAREALAND.map[a[q]]<-Population30min$landscan2010.asc[a[q]]
                sumgrid<-sumgrid+sum(popurban_grid$GAREALAND.map[a[q]],na.rm=TRUE)
                b<-b+1
                urban_grid$GAREALAND.map[a[q]]<-1
              } else {
                popurban_grid$GAREALAND.map[a[q]]<-(CountryDataPoint$pop_urb_num[i]-sumgrid)/length(q)
                urban_grid$GAREALAND.map[a[q]]<-2
                poprural_grid$GAREALAND.map[a[q]]<-Population30min$landscan2010.asc[a[q]]-popurban_grid$GAREALAND.map[a[q]]
                break
              }
            }
          } else {
            b<-b+1
          }
        }
      }
    } else popurban_grid$GAREALAND.map[a]<-NA
  }  
#  write(i,file="")
}

rm(i,a,b)

a<-is.na(popurban_grid$GAREALAND.map) #&& !is.na(population_km2$GAREALAND.map)
poprural_grid$GAREALAND.map[a]<-Population30min$landscan2010.asc[a]

#plotjes om tussendoor wat dingen te bekijken
b<-raster(popurban_grid)
plot(b,  main="popurban_grid")
c<-raster(urban_grid)
plot(c, col=colors, main="urban_grid")
d<-raster(poprural_grid)
plot(d, main= "poprural_grid")
ff<-raster(Population30min)
plot(ff, main= "Population 30 min")

#nu van populatie naar Cryptosporidium
for (i in 1:length(CountryDataPoint$iso)){
  h<-which(Countries30minNoStates$Countries30minNoStates.asc==CountryDataPoint$iso[i])
  if (is.na(point$crypto_urb_pp[i] )){
#    print(i)
  } else {
    h1<-which(urban_grid$GAREALAND.map[h]==1)
    crypto_grid$GAREALAND.map[h[h1]]<-        (popurban_grid$GAREALAND.map[h[h1]]*point$crypto_urb_pp[i])
    crypto_grid_under5$GAREALAND.map[h[h1]]<- (popurban_grid$GAREALAND.map[h[h1]]*point$crypto_urb_pp_under5[i])
    crypto_grid_others$GAREALAND.map[h[h1]]<- (popurban_grid$GAREALAND.map[h[h1]]*point$crypto_urb_pp_others[i])
    crypto_grid_diffuse$GAREALAND.map[h[h1]]<- (popurban_grid$GAREALAND.map[h[h1]]*point$crypto_urb_dif_pp[i])
    h2<-which(urban_grid$GAREALAND.map[h]==2)
    crypto_grid$GAREALAND.map[h[h2]]<-        (popurban_grid$GAREALAND.map[h[h2]]*point$crypto_urb_pp[i])+(poprural_grid$GAREALAND.map[h[h2]]*point$crypto_rur_pp[i])  
    crypto_grid_under5$GAREALAND.map[h[h2]]<- (popurban_grid$GAREALAND.map[h[h2]]*point$crypto_urb_pp_under5[i])+(poprural_grid$GAREALAND.map[h[h2]]*point$crypto_rur_pp_under5[i])
    crypto_grid_others$GAREALAND.map[h[h2]]<- (popurban_grid$GAREALAND.map[h[h2]]*point$crypto_urb_pp_others[i])+(poprural_grid$GAREALAND.map[h[h2]]*point$crypto_rur_pp_others[i])
    crypto_grid_diffuse$GAREALAND.map[h[h2]]<-(popurban_grid$GAREALAND.map[h[h2]]*point$crypto_urb_dif_pp[i])+(poprural_grid$GAREALAND.map[h[h2]]*point$crypto_rur_dif_pp[i])
    h3<-is.na(urban_grid$GAREALAND.map[h])
    crypto_grid$GAREALAND.map[h[h3]]<-        (poprural_grid$GAREALAND.map[h[h3]]*point$crypto_rur_pp[i])  
    crypto_grid_under5$GAREALAND.map[h[h3]]<- (poprural_grid$GAREALAND.map[h[h3]]*point$crypto_rur_pp_under5[i]) 
    crypto_grid_others$GAREALAND.map[h[h3]]<- (poprural_grid$GAREALAND.map[h[h3]]*point$crypto_rur_pp_others[i]) 
    crypto_grid_diffuse$GAREALAND.map[h[h3]]<-(poprural_grid$GAREALAND.map[h[h3]]*point$crypto_rur_dif_pp[i]) 
    }
} 
rm(h,i)

#plotjes bekijken
oo<-raster(crypto_grid)
plot(oo, col=colors, main="crypto_grid")
logoo<-log10(oo)
plot(logoo,col=colors,main="crypto grid log")
writeRaster(oo,paste0(working.path.out,"humanemissions",scenario),overwrite=TRUE, format="ascii")

oo_under5<-raster(crypto_grid_under5)
logoo_under5<-log10(oo_under5)

oo_others<-raster(crypto_grid_others)
logoo_others<-log10(oo_others)

oo_diffuse<-raster(crypto_grid_diffuse)
plot(oo_diffuse, col=colors, main="crypto_grid diffuse")
logoo_diffuse<-log10(oo_diffuse)
plot(logoo_diffuse,col=colors,main="crypto grid diffuse log")
writeRaster(oo_diffuse,paste0(working.path.out,"humanemissions_diffuse",scenario),overwrite=TRUE, format="ascii")

##plotting


#breaks definiëren om als label bij te kunnen voegen
brks1<-c(5,6,7,8,9,10,11,12,13,14,15,16)
brks2<-c(5,7,9,11,13,15,17)

#geschikt kleurpalet definiëren
#colors8<-c("lightslateblue","slateblue4","blue","dodgerblue", "lightseagreen", "limegreen", "olivedrab1", "yellow", "orange","red","red4")
colors8<-c("slateblue4","slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")

#load country border data 
data(countriesHigh)


#Single plot
tiff(paste(working.path.out,"Testplot_crypto_all",scenario,".tiff",sep=""), width=60, height=30, units = "cm", pointsize=30, res=300) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logoo, col=colors8, breaks=brks1,legend=FALSE, axes=FALSE) #xlab="Longitude (degrees)", ylab="Latitude (degrees)",
plot(countriesHigh, add=TRUE)

plot(logoo, legend.only=TRUE, col=colors8,breaks=brks1,horizontal=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=brks1,
                    labels=brks1, 
                    cex.axis=0.6,font=1.4),
     legend.args=list(text='Cryptosporidium emissions (log10 oocysts / grid / year)', font=1.8, cex=0.8)) #side=4, line=2.5, 
dev.off()

tiff(paste0("Testplot__crypto_under5",scenario,".tiff"), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logoo_under5, col=colors8, xlab="Longitude (degrees)", ylab="Latitude (degrees)", main="log Cryptosporidium / year")
plot(countriesHigh, add=TRUE)
dev.off()

tiff(paste0("Testplot_crypto_others",scenario,".tiff"), width=60, height=30, units = "cm", pointsize=30, res=100) #put resolution at 300 for high quality image for publication
par(lwd=1) #omi = c(0.5, 0.5, 0.5, 1.3), mai=c(2,2,1,0.5), par(mai=c(2,0.5,1,2))
plot(logoo_others, col=colors8, xlab="Longitude (degrees)", ylab="Latitude (degrees)", main="log Cryptosporidium / year")
plot(countriesHigh, add=TRUE)
dev.off()


mtext("log Cryptosporidium / year", 4)


################################# pie charts ################################


#make dataframe to fill with emissions in different categories
k<-length(CountryDataPoint$iso)
pie<-array(dim=c(k,19))
colnames(pie)<-c("countryname","iso","con_urb_und5","con_urb_others","dir_urb_und5", "dir_urb_others", "con_rur_und5", "con_rur_others", "dir_rur_und5", "dir_rur_others", "dif_rur_und5", "dif_rur_others", "con_urb_total", "dir_urb_total","con_rur_total", "dir_rur_total", "dif_rur_total","und5_total","others_total" )
rm(k)
pie<-as.data.frame(pie)

#find developed and developing countries & identify excretion categories
#a = developing, b=developed
#c1=africa, c2=asia, c3=rest of the world
#identify countries that fall into the different excretion categories
ac2<-which(CountryDataPoint$hdi <= HDIboundary) #low HDI
bc2<-which(CountryDataPoint$hdi > HDIboundary)  #high HDI


#For <5:
#bc2 en bc3 moeten alles 1/2 nappy correction krijgen
#ac1, ac2 en ac3 moeten alleen nappy correction krijgen voor sewage

#fill dataframe
pie$countryname<-CountryDataPoint$countryname
pie$iso<-CountryDataPoint$iso

#for HDI high
for (i in 1:length(CountryDataPoint$iso[bc2])){
#  print(i)
  pie$con_urb_und5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_urb_under5[bc2[i]]*
                                        CountryDataPoint$con_urb[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]])  #urban connected
  pie$dir_urb_und5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_urb_under5[bc2[i]]*
                                        CountryDataPoint$dir_urb[bc2[i]]*UrbanDirect2Water)                         #urban direct
  
#  print(i)
  
  pie$con_rur_und5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_rur_under5[bc2[i]]*
                                        CountryDataPoint$con_rur[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]])#rural connected
  
  pie$dir_rur_und5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_rur_under5[bc2[i]]*
                                        CountryDataPoint$dir_rur[bc2[i]]*RuralDirect2Water)                       #rural direct
  
  
  pie$dif_rur_und5[bc2[i]]<-NappyCor*(cryptoExcretionHighHDI_child*CountryDataPoint$pop_rur_under5[bc2[i]]*
                                        CountryDataPoint$dif_rur[bc2[i]]*RuralDiffuse2Water)             #rural diffuse
#  print(i)
  
  
  
  
  pie$con_urb_others[bc2[i]]<-(cryptoExcretionHighHDI_others*(CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])*
                                 CountryDataPoint$con_urb[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]])  #urban connected
  
  
  pie$dir_urb_others[bc2[i]]<-(cryptoExcretionHighHDI_others*(CountryDataPoint$pop_urb_5to14[bc2[i]]+CountryDataPoint$pop_urb_15to24[bc2[i]]+CountryDataPoint$pop_urb_25plus[bc2[i]])*
                                 CountryDataPoint$dir_urb[bc2[i]]*UrbanDirect2Water)                         #urban direct
  
#  print(i)
  
  pie$con_rur_others[bc2[i]]<-(cryptoExcretionHighHDI_others*(CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])*
                                 CountryDataPoint$con_rur[bc2[i]]*CountryDataPoint$removalfraction[bc2[i]])  #rural connected
  
  pie$dir_rur_others[bc2[i]]<-(cryptoExcretionHighHDI_others*(CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])*
                                 CountryDataPoint$dir_rur[bc2[i]]*RuralDirect2Water)                         #rural direct
  
  pie$dif_rur_others[bc2[i]]<-(cryptoExcretionHighHDI_others*(CountryDataPoint$pop_rur_5to14[bc2[i]]+CountryDataPoint$pop_rur_15to24[bc2[i]]+CountryDataPoint$pop_rur_25plus[bc2[i]])*
                                 CountryDataPoint$dif_rur[bc2[i]]*RuralDiffuse2Water)                        #rural diffuse
#  print(i)
  
  pie$con_urb_total[bc2[i]]<- pie$con_urb_und5[bc2[i]]+pie$con_urb_others[bc2[i]]
  pie$dir_urb_total[bc2[i]]<- pie$dir_urb_und5[bc2[i]]+pie$dir_urb_others[bc2[i]]
  pie$con_rur_total[bc2[i]]<- pie$con_rur_und5[bc2[i]]+pie$con_rur_others[bc2[i]]
  pie$dir_rur_total[bc2[i]]<- pie$dir_rur_und5[bc2[i]]+pie$dir_rur_others[bc2[i]]
  pie$dif_rur_total[bc2[i]]<- pie$dif_rur_und5[bc2[i]]+pie$dif_rur_others[bc2[i]]
  pie$und5_total[bc2[i]]  <- pie$con_urb_und5[bc2[i]]+pie$dir_urb_und5[bc2[i]]+ pie$con_rur_und5[bc2[i]]+pie$dir_rur_und5[bc2[i]]+pie$dif_rur_und5[bc2[i]]
  pie$others_total[bc2[i]]<- pie$con_urb_others[bc2[i]]+pie$dir_urb_others[bc2[i]]+pie$con_rur_others[bc2[i]]+pie$dir_rur_others[bc2[i]]+pie$dif_rur_others[bc2[i]]
  
}
rm(i)

#for HDI low
#nappy correction alleen bij de under 5s die riolering hebben
for (i in 1:length(CountryDataPoint$iso[ac2])){
#  print(i)
  pie$con_urb_und5[ac2[i]]<-NappyCor*(cryptoExcretionLowHDI_child*CountryDataPoint$pop_urb_under5[ac2[i]]*
                                        CountryDataPoint$con_urb[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]])  #urban connected
  pie$dir_urb_und5[ac2[i]]<-(cryptoExcretionLowHDI_child*CountryDataPoint$pop_urb_under5[ac2[i]]*
                               CountryDataPoint$dir_urb[ac2[i]]*UrbanDirect2Water)                         #urban direct
  
#  print(i)
  
  pie$con_rur_und5[ac2[i]]<-NappyCor*(cryptoExcretionLowHDI_child*CountryDataPoint$pop_rur_under5[ac2[i]]*
                                        CountryDataPoint$con_rur[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]])#rural connected
  
  pie$dir_rur_und5[ac2[i]]<-(cryptoExcretionLowHDI_child*CountryDataPoint$pop_rur_under5[ac2[i]]*
                               CountryDataPoint$dir_rur[ac2[i]]*RuralDirect2Water)                       #rural direct
  
  
  pie$dif_rur_und5[ac2[i]]<-(cryptoExcretionLowHDI_child*CountryDataPoint$pop_rur_under5[ac2[i]]*
                               CountryDataPoint$dif_rur[ac2[i]]*RuralDiffuse2Water)             #rural diffuse
#  print(i)
  
  
  
  
  pie$con_urb_others[ac2[i]]<-(cryptoExcretionLowHDI_others*(CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])*
                                 CountryDataPoint$con_urb[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]])  #urban connected
  
  
  pie$dir_urb_others[ac2[i]]<-(cryptoExcretionLowHDI_others*(CountryDataPoint$pop_urb_5to14[ac2[i]]+CountryDataPoint$pop_urb_15to24[ac2[i]]+CountryDataPoint$pop_urb_25plus[ac2[i]])*
                                 CountryDataPoint$dir_urb[ac2[i]]*UrbanDirect2Water)                         #urban direct
  
#  print(i)
  
  pie$con_rur_others[ac2[i]]<-(cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])*
                                 CountryDataPoint$con_rur[ac2[i]]*CountryDataPoint$removalfraction[ac2[i]])  #rural connected
  
  pie$dir_rur_others[ac2[i]]<-(cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])*
                                 CountryDataPoint$dir_rur[ac2[i]]*RuralDirect2Water)                         #rural direct
  
  pie$dif_rur_others[ac2[i]]<-(cryptoExcretionLowHDI_others*(CountryDataPoint$pop_rur_5to14[ac2[i]]+CountryDataPoint$pop_rur_15to24[ac2[i]]+CountryDataPoint$pop_rur_25plus[ac2[i]])*
                                 CountryDataPoint$dif_rur[ac2[i]]*RuralDiffuse2Water)                        #rural diffuse
#  print(i)
  
  pie$con_urb_total[ac2[i]]<- pie$con_urb_und5[ac2[i]]+pie$con_urb_others[ac2[i]]
  pie$dir_urb_total[ac2[i]]<- pie$dir_urb_und5[ac2[i]]+pie$dir_urb_others[ac2[i]]
  pie$con_rur_total[ac2[i]]<- pie$con_rur_und5[ac2[i]]+pie$con_rur_others[ac2[i]]
  pie$dir_rur_total[ac2[i]]<- pie$dir_rur_und5[ac2[i]]+pie$dir_rur_others[ac2[i]]
  pie$dif_rur_total[ac2[i]]<- pie$dif_rur_und5[ac2[i]]+pie$dif_rur_others[ac2[i]]
  pie$und5_total[ac2[i]]<- pie$con_urb_und5[ac2[i]]+pie$dir_urb_und5[ac2[i]]+ pie$con_rur_und5[ac2[i]]+pie$dir_rur_und5[ac2[i]]+pie$dif_rur_und5[ac2[i]]
  pie$others_total[ac2[i]]<- pie$con_urb_others[ac2[i]]+pie$dir_urb_others[ac2[i]]+ pie$con_rur_others[ac2[i]]+pie$dir_rur_others[ac2[i]]+pie$dif_rur_others[ac2[i]]
  
}
rm(i)



#nu pie charts maken, kies eerst van welke landen
aa<-which(pie$iso==826) #UK
bb<-which(pie$iso==566) #Nigeria

#eerst van under5s / others
slices1<-c(pie$und5_total[aa],pie$others_total[aa])
slices2<-c(pie$und5_total[bb],pie$others_total[bb])
u5aa<-paste("<5 ",round(100*pie$und5_total[aa]/(pie$und5_total[aa]+pie$others_total[aa])),"%",sep="")
a5aa<-paste("5+ ",round(100*pie$others_total[aa]/(pie$und5_total[aa]+pie$others_total[aa])),"%",sep="")
u5bb<-paste("<5 ",round(100*pie$und5_total[bb]/(pie$und5_total[bb]+pie$others_total[bb])),"%",sep="")
a5bb<-paste("5+ ",round(100*pie$others_total[bb]/(pie$und5_total[bb]+pie$others_total[bb])),"%",sep="")
lblaa<-c(u5aa,a5aa)  
lblbb<-c(u5bb,a5bb)

piecolors2<-c("red", "blue")

#plot to view, for exporting changes here need to be copied below
pie1<-pie(slices1,labels=lblaa,col=piecolors2,main="Cryptosporidium emissions in the UK \nfrom the population below and above 5 years")
pie2<-pie(slices2,labels=lblbb,col=piecolors2,main="Cryptosporidium emissions in Nigeria \nfrom the population below and above 5 years")


#nu van de verschillende bronnen
slices3<-c(pie$con_urb_total[aa],pie$con_rur_total[aa]) #pie$dir_urb_total[aa],,pie$dir_rur_total[aa],pie$dif_rur_total[aa] ) 
slices4<-c(pie$con_urb_total[bb],pie$dir_urb_total[bb],pie$con_rur_total[bb],pie$dir_rur_total[bb],pie$dif_rur_total[bb] ) 
ucaa<-paste("Urban connected ",round(100*pie$con_urb_total[aa]/(pie$con_urb_total[aa]+pie$con_rur_total[aa])),"%",sep="")
rcaa<-paste("Rural connected ",round(100*pie$con_rur_total[aa]/(pie$con_urb_total[aa]+pie$con_rur_total[aa])),"%",sep="")
ucbb<-paste("Urban connected ",round(100*pie$con_urb_total[bb]/(pie$con_urb_total[bb]+pie$dir_urb_total[bb]+pie$con_rur_total[bb]+pie$dir_rur_total[bb]+pie$dif_rur_total[bb])),"%",sep="")
udbb<-paste("Urban direct ",round(100*pie$dir_urb_total[bb]/(pie$con_urb_total[bb]+pie$dir_urb_total[bb]+pie$con_rur_total[bb]+pie$dir_rur_total[bb]+pie$dif_rur_total[bb])),"%",sep="")
rcbb<-paste("Rural connected ",round(100*pie$con_rur_total[bb]/(pie$con_urb_total[bb]+pie$dir_urb_total[bb]+pie$con_rur_total[bb]+pie$dir_rur_total[bb]+pie$dif_rur_total[bb])),"%",sep="")
rdirbb<-paste("Rural direct ", round(100*pie$dir_rur_total[bb]/(pie$con_urb_total[bb]+pie$dir_urb_total[bb]+pie$con_rur_total[bb]+pie$dir_rur_total[bb]+pie$dif_rur_total[bb])),"%",sep="")
rdifbb<-paste("Rural diffuse ", round(100*pie$dif_rur_total[bb]/(pie$con_urb_total[bb]+pie$dir_urb_total[bb]+pie$con_rur_total[bb]+pie$dir_rur_total[bb]+pie$dif_rur_total[bb])),"%",sep="")
lbl2<-c(ucbb,udbb,rcbb,rdirbb,rdifbb)
lbl3<-c(ucaa,rcaa)
piecolors5<-c("red","orange","blue","seagreen","cyan")

#het label van UK is nu wat gek, omdat het eigenlijk 5 slices heeft, maar twee zijn er nul
pie3<-pie(slices3,labels=lbl3,col=piecolors2,main="Cryptosporidium emissions \nfrom different sources in the UK")
pie4<-pie(slices4,labels=lbl2,col=piecolors5,main="Cryptosporidium emissions \nfrom different sources in Nigeria")


tiff(paste0("pie1crypto",scenario,".tiff"), width=15, height=10, units = "cm", res=100) #put resolution at 300 for high quality image for publication
pie(slices1,labels=lblaa,col=piecolors2,main="Cryptosporidium emissions in the UK \nfrom the population below and above 5 years")
dev.off()

tiff(paste0("pie2crypto",scenario,".tiff"), width=15, height=10, units = "cm", res=100) #put resolution at 300 for high quality image for publication
pie(slices2,labels=lblbb,col=piecolors2,main="Cryptosporidium emissions in Nigeria \nfrom the population below and above 5 years")
dev.off()

tiff(paste0("pie3crypto",scenario,".tiff"), width=15, height=10, units = "cm", res=100) #put resolution at 300 for high quality image for publication
pie3<-pie(slices3,labels=lbl3,col=piecolors5,main="Cryptosporidium emissions from \n sources in the UK")
dev.off()

tiff(paste0("pie4crypto",scenario,".tiff"), width=15, height=10, units = "cm", res=100) #put resolution at 300 for high quality image for publication
pie4<-pie(slices4,labels=lbl2,col=piecolors5,main="Cryptosporidium emissions from \ndifferent sources in Nigeria")
dev.off()

