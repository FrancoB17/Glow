## Cryptosporidium model - diffuse sources
## by Lucie
## last modified September 2016
# this script is called by calling_diffuse_emissions.R
# it is rather long mainly because some things are repeated for different subsets 
# (e.g. split for animal species, regions, or land and storage with and without decay during storage)


###### read in fractions surviving storage rasters ######
Fcattle_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_survivingstorage.asc")
Fbuffaloes_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_survivingstorage.asc")
Fpigs_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_survivingstorage.asc")
Fpoultry_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpoultry_survivingstorage.asc")
Fsheep_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_survivingstorage.asc")
Fgoats_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_survivingstorage.asc")
Fhorses_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_survivingstorage.asc")
Fasses_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_survivingstorage.asc")
Fmules_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_survivingstorage.asc")
Fcamels_survivingstorage  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_survivingstorage.asc")

#for the scenario that everything goes to mesophilic anaerobic digesters
# the following statement is built in
if (par$digester_scenario_meso[i] == 1){
  Fcattle_survivingstorage  <- 0.01
  Fbuffaloes_survivingstorage  <- 0.01 
  Fpigs_survivingstorage  <- 0.01
  Fpoultry_survivingstorage  <- 0.01
  Fsheep_survivingstorage  <- 0.01
  Fgoats_survivingstorage  <- 0.01
  Fhorses_survivingstorage  <- 0.01
  Fasses_survivingstorage  <- 0.01
  Fmules_survivingstorage  <- 0.01
  Fcamels_survivingstorage  <- 0.01
  
}

#for the scenario that everything goes to thermophilic anaerobic digesters
# the following statement is built in
if (par$digester_scenario_thermo[i] == 1){
  Fcattle_survivingstorage  <- 0.00001
  Fbuffaloes_survivingstorage  <- 0.00001 
  Fpigs_survivingstorage  <- 0.00001
  Fpoultry_survivingstorage  <- 0.00001
  Fsheep_survivingstorage  <- 0.00001
  Fgoats_survivingstorage  <- 0.00001
  Fhorses_survivingstorage  <- 0.00001
  Fasses_survivingstorage  <- 0.00001
  Fmules_survivingstorage  <- 0.00001
  Fcamels_survivingstorage  <- 0.00001
  
}

print(i)
###### read in int/ext to land/ to storage rasters ###########


#Read in rasters with fraction to storage and fractions to land
#change values below zero to zero
#change values above 1 to 1
Fcattle_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_SI.asc")+par$fraction_tostorage[i]
Fcattle_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_LI.asc")+par$fraction_toland[i]
Fcattle_O<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_O.asc")
Fcattle_max<-1-Fcattle_O
values(Fcattle_S) <- ifelse(values(Fcattle_S)<0,0,ifelse(values(Fcattle_S)>values(Fcattle_max),values(Fcattle_max),values(Fcattle_S)))
values(Fcattle_L) <- ifelse(values(Fcattle_L)<0,0,ifelse(values(Fcattle_L)>values(Fcattle_max),values(Fcattle_max),values(Fcattle_L)))

Fbuffaloes_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_SI.asc")+par$fraction_tostorage[i]
Fbuffaloes_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_LI.asc")+par$fraction_toland[i]
Fbuffaloes_O<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_O.asc")
Fbuffaloes_max<-1-Fbuffaloes_O
values(Fbuffaloes_S) <- ifelse(values(Fbuffaloes_S)<0,0,ifelse(values(Fbuffaloes_S)>values(Fbuffaloes_max),values(Fbuffaloes_max),values(Fbuffaloes_S)))
values(Fbuffaloes_L) <- ifelse(values(Fbuffaloes_L)<0,0,ifelse(values(Fbuffaloes_L)>values(Fbuffaloes_max),values(Fbuffaloes_max),values(Fbuffaloes_L)))

Fpigs_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_SI.asc")+par$fraction_tostorage[i]
Fpigs_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_LI.asc")+par$fraction_toland[i]
Fpigs_O<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_O.asc")
Fpigs_max<-1-Fpigs_O
values(Fpigs_S) <- ifelse(values(Fpigs_S)<0,0,ifelse(values(Fpigs_S)>values(Fpigs_max),values(Fpigs_max),values(Fpigs_S)))
values(Fpigs_L) <- ifelse(values(Fpigs_L)<0,0,ifelse(values(Fpigs_L)>values(Fpigs_max),values(Fpigs_max),values(Fpigs_L)))

#for sheep, other uses are zero
Fsheep_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_SI.asc")+par$fraction_tostorage[i]
Fsheep_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_LI.asc")+par$fraction_toland[i]
values(Fsheep_S) <- ifelse(values(Fsheep_S)<0,0,ifelse(values(Fsheep_S)>1,1,values(Fsheep_S)))
values(Fsheep_L) <- ifelse(values(Fsheep_L)<0,0,ifelse(values(Fsheep_L)>1,1,values(Fsheep_L)))

#for goats, other uses are zero
Fgoats_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_SI.asc")+par$fraction_tostorage[i]
Fgoats_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_LI.asc")+par$fraction_toland[i]
values(Fgoats_S) <- ifelse(values(Fgoats_S)<0,0,ifelse(values(Fgoats_S)>1,1,values(Fgoats_S)))
values(Fgoats_L) <- ifelse(values(Fgoats_L)<0,0,ifelse(values(Fgoats_L)>1,1,values(Fgoats_L)))

Fchickens_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_SI.asc")+par$fraction_tostorage[i]
Fchickens_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_LI.asc")+par$fraction_toland[i]
Fchickens_O<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_O.asc")
Fchickens_max<-1-Fchickens_O
values(Fchickens_S) <- ifelse(values(Fchickens_S)<0,0,ifelse(values(Fchickens_S)>values(Fchickens_max),values(Fchickens_max),values(Fchickens_S)))
values(Fchickens_L) <- ifelse(values(Fchickens_L)<0,0,ifelse(values(Fchickens_L)>values(Fchickens_max),values(Fchickens_max),values(Fchickens_L)))

Fducks_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_SI.asc")+par$fraction_tostorage[i]
Fducks_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_LI.asc")+par$fraction_toland[i]
Fducks_O<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_O.asc")
Fducks_max<-1-Fducks_O
values(Fducks_S) <- ifelse(values(Fducks_S)<0,0,ifelse(values(Fducks_S)>values(Fducks_max),values(Fducks_max),values(Fducks_S)))
values(Fducks_L) <- ifelse(values(Fducks_L)<0,0,ifelse(values(Fducks_L)>values(Fducks_max),values(Fducks_max),values(Fducks_L)))

#for horses, camels, mules and asses, other uses are zero
Fhorses_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_SI.asc")+par$fraction_tostorage[i]
Fhorses_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_LI.asc")+par$fraction_toland[i]
values(Fhorses_S) <- ifelse(values(Fhorses_S)<0,0,ifelse(values(Fhorses_S)>1,1,values(Fhorses_S)))
values(Fhorses_L) <- ifelse(values(Fhorses_L)<0,0,ifelse(values(Fhorses_L)>1,1,values(Fhorses_L)))

Fcamels_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_SI.asc")+par$fraction_tostorage[i]
Fcamels_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_LI.asc")+par$fraction_toland[i]
values(Fcamels_S) <- ifelse(values(Fcamels_S)<0,0,ifelse(values(Fcamels_S)>1,1,values(Fcamels_S)))
values(Fcamels_L) <- ifelse(values(Fcamels_L)<0,0,ifelse(values(Fcamels_L)>1,1,values(Fcamels_L)))

Fmules_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_SI.asc")+par$fraction_tostorage[i]
Fmules_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_LI.asc")+par$fraction_toland[i]
values(Fmules_S) <- ifelse(values(Fmules_S)<0,0,ifelse(values(Fmules_S)>1,1,values(Fmules_S)))
values(Fmules_L) <- ifelse(values(Fmules_L)<0,0,ifelse(values(Fmules_L)>1,1,values(Fmules_L)))

Fasses_S<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_SE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_SI.asc")+par$fraction_tostorage[i]
Fasses_L<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_LE.asc")+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_LI.asc")+par$fraction_toland[i]
values(Fasses_S) <- ifelse(values(Fasses_S)<0,0,ifelse(values(Fasses_S)>1,1,values(Fasses_S)))
values(Fasses_L) <- ifelse(values(Fasses_L)<0,0,ifelse(values(Fasses_L)>1,1,values(Fasses_L)))

######### calculations going to land ##########


#Calculate surviving going to land after storage 
Fcattle_S_survived <- Fcattle_S * Fcattle_survivingstorage
Fbuffaloes_S_survived <- Fbuffaloes_S * Fbuffaloes_survivingstorage
Fpigs_S_survived <- Fpigs_S * Fpigs_survivingstorage
Fchickens_S_survived <- Fchickens_S * Fpoultry_survivingstorage
Fducks_S_survived <- Fducks_S * Fpoultry_survivingstorage
Fsheep_S_survived <- Fsheep_S * Fsheep_survivingstorage
Fgoats_S_survived <- Fgoats_S * Fgoats_survivingstorage
Fhorses_S_survived <- Fhorses_S * Fhorses_survivingstorage
Fasses_S_survived <- Fasses_S * Fasses_survivingstorage
Fmules_S_survived <- Fmules_S * Fmules_survivingstorage
Fcamels_S_survived <- Fcamels_S * Fcamels_survivingstorage

#compute total fraction ending up on land:
Fcattle_toland    <- Fcattle_S_survived + Fcattle_L
Fbuffaloes_toland <- Fbuffaloes_S_survived + Fbuffaloes_L
Fpigs_toland      <- Fpigs_S_survived + Fpigs_L
Fchickens_toland  <- Fchickens_S_survived + Fchickens_L
Fducks_toland     <- Fducks_S_survived + Fducks_L
Fsheep_toland     <- Fsheep_S_survived + Fsheep_L
Fgoats_toland     <- Fgoats_S_survived + Fgoats_L
Fhorses_toland    <- Fhorses_S_survived + Fhorses_L
Fasses_toland     <- Fasses_S_survived + Fasses_L
Fmules_toland     <- Fmules_S_survived + Fmules_L
Fcamels_toland    <- Fcamels_S_survived + Fcamels_L


####### Read in heads rasters ########
cattleheads  <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/cattleheads.grd")*par$animal_numbers[i]
chickenheads <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/chickenheads.grd")*par$animal_numbers[i]
duckheads    <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/duckheads.grd")*par$animal_numbers[i]
goatheads    <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/goatheads.grd")*par$animal_numbers[i]
sheepheads   <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/sheepheads.grd")*par$animal_numbers[i]
pigheads     <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/pigheads.grd")*par$animal_numbers[i]
buffaloheads <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/buffaloheads.grd")*par$animal_numbers[i]
horseheads   <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/horseheads.grd")*par$animal_numbers[i]
assheads     <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/assheads.grd")*par$animal_numbers[i]
camelheads   <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/camelheads.grd")*par$animal_numbers[i]
muleheads    <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/animals/muleheads.grd")*par$animal_numbers[i]

### manure production per animal (kg/year) #####

regioncodes<-read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/regioncodes.csv")

#add columns to the table regioncodes and calculate values from parameters from the par file
regioncodes$manure_cattle_adult<-NA
regioncodes$manure_cattle_adult[1]<-par$mass_cattle_africa[i]*par$manure_per1000kgmass_cattle[i]*365  
regioncodes$manure_cattle_adult[2]<-par$mass_cattle_asia[i]*par$manure_per1000kgmass_cattle[i]*365      
regioncodes$manure_cattle_adult[3]<-par$mass_cattle_europe[i]*par$manure_per1000kgmass_cattle[i]*365      
regioncodes$manure_cattle_adult[4]<-par$mass_cattle_latinamerica[i]*par$manure_per1000kgmass_cattle[i]*365      
regioncodes$manure_cattle_adult[5]<-par$mass_cattle_mena[i]*par$manure_per1000kgmass_cattle[i]*365      
regioncodes$manure_cattle_adult[6]<-par$mass_cattle_northamerica[i]*par$manure_per1000kgmass_cattle[i]*365      
regioncodes$manure_cattle_adult[7]<-par$mass_cattle_oceania[i]*par$manure_per1000kgmass_cattle[i]*365     

#reading in region raster
# codes in this file are:
# Africa = 1
# Asia = 2
# Europe = 3
# Latin America = 4
# Middle East North Africa = 5
# North America = 6
# Oceania = 7
regionraster<- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/regionraster.asc")

#make raster of the manure cattle adult data
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$manure_cattle_adult)
manure_cattle_adult<-subs(regionraster,dataregions,subsWithNA=T)
#plot(manure_cattle_adult)

# Nu voor goats, sheep, horses en pigs onderscheid maken developing industrialized

regioncodes$manure_goats_adult<-NA
regioncodes$manure_goats_adult[1]<-par$mass_goats_dev[i]*par$manure_per1000kgmass_goats[i]*365  
regioncodes$manure_goats_adult[2]<-par$mass_goats_dev[i]*par$manure_per1000kgmass_goats[i]*365      
regioncodes$manure_goats_adult[3]<-par$mass_goats_ind[i]*par$manure_per1000kgmass_goats[i]*365      
regioncodes$manure_goats_adult[4]<-par$mass_goats_dev[i]*par$manure_per1000kgmass_goats[i]*365      
regioncodes$manure_goats_adult[5]<-par$mass_goats_dev[i]*par$manure_per1000kgmass_goats[i]*365      
regioncodes$manure_goats_adult[6]<-par$mass_goats_ind[i]*par$manure_per1000kgmass_goats[i]*365      
regioncodes$manure_goats_adult[7]<-par$mass_goats_ind[i]*par$manure_per1000kgmass_goats[i]*365  
#make raster of the manure  goats adult data
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$manure_goats_adult)
manure_goats_adult<-subs(regionraster,dataregions,subsWithNA=T)
#plot(manure_goats_adult)

regioncodes$manure_sheep_adult<-NA
regioncodes$manure_sheep_adult[1]<-par$mass_sheep_dev[i]*par$manure_per1000kgmass_sheep[i]*365  
regioncodes$manure_sheep_adult[2]<-par$mass_sheep_dev[i]*par$manure_per1000kgmass_sheep[i]*365      
regioncodes$manure_sheep_adult[3]<-par$mass_sheep_ind[i]*par$manure_per1000kgmass_sheep[i]*365      
regioncodes$manure_sheep_adult[4]<-par$mass_sheep_dev[i]*par$manure_per1000kgmass_sheep[i]*365      
regioncodes$manure_sheep_adult[5]<-par$mass_sheep_dev[i]*par$manure_per1000kgmass_sheep[i]*365      
regioncodes$manure_sheep_adult[6]<-par$mass_sheep_ind[i]*par$manure_per1000kgmass_sheep[i]*365      
regioncodes$manure_sheep_adult[7]<-par$mass_sheep_ind[i]*par$manure_per1000kgmass_sheep[i]*365  
#make raster of the manure  sheep adult data
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$manure_sheep_adult)
manure_sheep_adult<-subs(regionraster,dataregions,subsWithNA=T)
#plot(manure_sheep_adult)

regioncodes$manure_pigs_adult<-NA
regioncodes$manure_pigs_adult[1]<-par$mass_pigs_dev[i]*par$manure_per1000kgmass_pigs[i]*365  
regioncodes$manure_pigs_adult[2]<-par$mass_pigs_dev[i]*par$manure_per1000kgmass_pigs[i]*365      
regioncodes$manure_pigs_adult[3]<-par$mass_pigs_ind[i]*par$manure_per1000kgmass_pigs[i]*365      
regioncodes$manure_pigs_adult[4]<-par$mass_pigs_dev[i]*par$manure_per1000kgmass_pigs[i]*365      
regioncodes$manure_pigs_adult[5]<-par$mass_pigs_dev[i]*par$manure_per1000kgmass_pigs[i]*365      
regioncodes$manure_pigs_adult[6]<-par$mass_pigs_ind[i]*par$manure_per1000kgmass_pigs[i]*365      
regioncodes$manure_pigs_adult[7]<-par$mass_pigs_ind[i]*par$manure_per1000kgmass_pigs[i]*365  
#make raster of the manure  pigs adult data
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$manure_pigs_adult)
manure_pigs_adult<-subs(regionraster,dataregions,subsWithNA=T)
#plot(manure_pigs_adult)

regioncodes$manure_horses<-NA
regioncodes$manure_horses[1]<-par$mass_horses_dev[i]*par$manure_per1000kgmass_horses[i]*365  
regioncodes$manure_horses[2]<-par$mass_horses_dev[i]*par$manure_per1000kgmass_horses[i]*365      
regioncodes$manure_horses[3]<-par$mass_horses_ind[i]*par$manure_per1000kgmass_horses[i]*365      
regioncodes$manure_horses[4]<-par$mass_horses_dev[i]*par$manure_per1000kgmass_horses[i]*365      
regioncodes$manure_horses[5]<-par$mass_horses_dev[i]*par$manure_per1000kgmass_horses[i]*365      
regioncodes$manure_horses[6]<-par$mass_horses_ind[i]*par$manure_per1000kgmass_horses[i]*365      
regioncodes$manure_horses[7]<-par$mass_horses_ind[i]*par$manure_per1000kgmass_horses[i]*365  
#make raster of the manure  horses adult data
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$manure_horses)
manure_horses<-subs(regionraster,dataregions,subsWithNA=T)
#plot(manure_horses)


#for the other animal species no rasters need to be made, just single numbers (kg/year)
#for chickens and ducks this is skipped, as we have the excretion in oocysts/animal/day
manure_buffaloes_adult<-par$mass_buffaloes[i]*par$manure_per1000kgmass_buffaloes[i]*365
manure_camels<-par$mass_camels[i]*par$manure_per1000kgmass_camels[i]*365
manure_mules<-par$mass_mules[i]*par$manure_per1000kgmass_mules[i]*365
manure_asses<-par$mass_asses[i]*par$manure_per1000kgmass_asses[i]*365



## manure production for young animals (of cattle, buffaloes, sheep, goats, pigs) in kg/year ###
manure_cattle_young<-par$mass_cattle_young[i]*par$manure_per1000kgmass_cattle[i]*365
manure_buffaloes_young<-par$mass_buffaloes_young[i]*par$manure_per1000kgmass_buffaloes[i]*365
manure_pigs_young<-par$mass_pigs_young[i]*par$manure_per1000kgmass_pigs[i]*365
manure_sheep_young<-par$mass_sheep_young[i]*par$manure_per1000kgmass_sheep[i]*365
manure_goats_young<-par$mass_goats_young[i]*par$manure_per1000kgmass_goats[i]*365



###### prevalence ##########


#add columns to the table regioncodes and fill with parameters from the par file
regioncodes$prev_cattle_young<-NA
regioncodes$prev_cattle_young[1]<-par$prev_cattle_young_africa[i]  
regioncodes$prev_cattle_young[2]<-par$prev_cattle_young_asia[i]  
regioncodes$prev_cattle_young[3]<-par$prev_cattle_young_europe[i]  
regioncodes$prev_cattle_young[4]<-par$prev_cattle_young_latinamerica[i]  
regioncodes$prev_cattle_young[5]<-par$prev_cattle_young_mena[i]  
regioncodes$prev_cattle_young[6]<-par$prev_cattle_young_northamerica[i]  
regioncodes$prev_cattle_young[7]<-par$prev_cattle_young_oceania[i]  
regioncodes$prev_cattle_adult<-NA
regioncodes$prev_cattle_adult[1]<-par$prev_cattle_adult_africa[i]  
regioncodes$prev_cattle_adult[2]<-par$prev_cattle_adult_asia[i]  
regioncodes$prev_cattle_adult[3]<-par$prev_cattle_adult_europe[i]  
regioncodes$prev_cattle_adult[4]<-par$prev_cattle_adult_latinamerica[i]  
regioncodes$prev_cattle_adult[5]<-par$prev_cattle_adult_mena[i]  
regioncodes$prev_cattle_adult[6]<-par$prev_cattle_adult_northamerica[i]  
regioncodes$prev_cattle_adult[7]<-par$prev_cattle_adult_oceania[i]  

#create prevalence rasters for cattle
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$prev_cattle_young)
prev_cattle_young<-subs(regionraster,dataregions,subsWithNA=T)
#plot(prev_cattle_young)
dataregions<-data.frame(codes=regioncodes$code,values=regioncodes$prev_cattle_adult)
prev_cattle_adult<-subs(regionraster,dataregions,subsWithNA=T)
#plot(prev_cattle_adult)


##### compute monthly oocyst emissions per animal species rasters #####
#factor 1000 is added to go from oocysts per gram to oocysts per kilogram

cattleemissions_young <- cattleheads * Fcattle_toland * par$fraction_young_cattle[i] * prev_cattle_young * par$exc_cattle_young[i]*1000 * manure_cattle_young
cattleemissions_adult <- cattleheads * Fcattle_toland * (1-par$fraction_young_cattle[i])* prev_cattle_adult * par$exc_cattle_adult[i]*1000 * manure_cattle_adult
cattleemissions <- cattleemissions_young + cattleemissions_adult

buffaloemissions_young <- buffaloheads * Fbuffaloes_toland * par$fraction_young_buffaloes[i] * par$prev_buffaloes_young[i] * par$exc_buffaloes_young[i]*1000 * manure_buffaloes_young
buffaloemissions_adult <- buffaloheads * Fbuffaloes_toland * (1-par$fraction_young_buffaloes[i])* par$prev_buffaloes_adult[i] * par$exc_buffaloes_adult[i] *1000* manure_buffaloes_adult
buffaloemissions <- buffaloemissions_young + buffaloemissions_adult

pigemissions_young <- pigheads * Fpigs_toland * par$fraction_young_pigs[i] * par$prev_pigs_young[i] * par$exc_pigs_young[i]*1000 * manure_pigs_young
pigemissions_adult <- pigheads * Fpigs_toland * (1-par$fraction_young_pigs[i])* par$prev_pigs_adult[i] * par$exc_pigs_adult[i]*1000 * manure_pigs_adult
pigemissions <- pigemissions_young + pigemissions_adult

goatemissions_young <- goatheads * Fgoats_toland * par$fraction_young_goats[i] * par$prev_goats_young[i] * par$exc_goats_young[i]*1000 * manure_goats_young
goatemissions_adult <- goatheads * Fgoats_toland * (1-par$fraction_young_goats[i])* par$prev_goats_adult[i] * par$exc_goats_adult[i]*1000 * manure_goats_adult
goatemissions <- goatemissions_young + goatemissions_adult

sheepemissions_young <- sheepheads * Fsheep_toland * par$fraction_young_sheep[i] * par$prev_sheep_young[i] * par$exc_sheep_young[i]*1000 * manure_sheep_young
sheepemissions_adult <- sheepheads * Fsheep_toland * (1-par$fraction_young_sheep[i])* par$prev_sheep_adult[i] * par$exc_sheep_adult[i]*1000 * manure_sheep_adult
sheepemissions <- sheepemissions_young + sheepemissions_adult

horseemissions <- horseheads * Fhorses_toland * par$prev_horses[i] *par$exc_horses[i]*1000* manure_horses
camelemissions <- camelheads * Fcamels_toland * par$prev_camels[i] * par$exc_camels[i] *1000* manure_camels
muleemissions <- muleheads * Fmules_toland * par$prev_mules[i] *par$exc_mules[i] *1000* manure_mules
assemissions <- assheads * Fasses_toland * par$prev_asses[i] *par$exc_asses[i]*1000* manure_asses

#for chickens and ducks the excretion is per day, not per manure
#that is why we do multiply with 365 (done elsewhere for the other animals) and do not multiply with manure
chickenemissions <- chickenheads * Fchickens_toland *par$prev_chickens[i] *par$exc_chickens_day[i]*365*1000
duckemissions <- duckheads * Fducks_toland * par$prev_ducks[i] *par$exc_ducks_day[i]*365*1000

####### compute emissions separately for direct to land #######

cattleemissions_young_L<- cattleheads * Fcattle_L * par$fraction_young_cattle[i] * prev_cattle_young * par$exc_cattle_young[i]*1000 * manure_cattle_young
cattleemissions_adult_L<- cattleheads * Fcattle_L * (1-par$fraction_young_cattle[i])* prev_cattle_adult * par$exc_cattle_adult[i]*1000 * manure_cattle_adult
cattleemissions_L<- cattleemissions_young_L + cattleemissions_adult_L 

buffaloemissions_young_L<- buffaloheads * Fbuffaloes_L * par$fraction_young_buffaloes[i] * par$prev_buffaloes_young[i] * par$exc_buffaloes_young[i]*1000 * manure_buffaloes_young
buffaloemissions_adult_L<- buffaloheads * Fbuffaloes_L * (1-par$fraction_young_buffaloes[i])* par$prev_buffaloes_adult[i] * par$exc_buffaloes_adult[i] *1000* manure_buffaloes_adult
buffaloemissions_L<- buffaloemissions_young_L  + buffaloemissions_adult_L 

pigemissions_young_L<- pigheads * Fpigs_L * par$fraction_young_pigs[i] * par$prev_pigs_young[i] * par$exc_pigs_young[i]*1000 * manure_pigs_young
pigemissions_adult_L<- pigheads * Fpigs_L * (1-par$fraction_young_pigs[i])* par$prev_pigs_adult[i] * par$exc_pigs_adult[i]*1000 * manure_pigs_adult
pigemissions_L<- pigemissions_young_L  + pigemissions_adult_L 

goatemissions_young_L<- goatheads * Fgoats_L * par$fraction_young_goats[i] * par$prev_goats_young[i] * par$exc_goats_young[i]*1000 * manure_goats_young
goatemissions_adult_L<- goatheads * Fgoats_L * (1-par$fraction_young_goats[i])* par$prev_goats_adult[i] * par$exc_goats_adult[i]*1000 * manure_goats_adult
goatemissions_L<- goatemissions_young_L  + goatemissions_adult_L 

sheepemissions_young_L<- sheepheads * Fsheep_L * par$fraction_young_sheep[i] * par$prev_sheep_young[i] * par$exc_sheep_young[i]*1000 * manure_sheep_young
sheepemissions_adult_L<- sheepheads * Fsheep_L * (1-par$fraction_young_sheep[i])* par$prev_sheep_adult[i] * par$exc_sheep_adult[i]*1000 * manure_sheep_adult
sheepemissions_L<- sheepemissions_young_L  + sheepemissions_adult_L 

horseemissions_L<- horseheads * Fhorses_L * par$prev_horses[i] *par$exc_horses[i]*1000* manure_horses
camelemissions_L<- camelheads * Fcamels_L * par$prev_camels[i] * par$exc_camels[i] *1000* manure_camels
muleemissions_L<- muleheads * Fmules_L * par$prev_mules[i] *par$exc_mules[i] *1000* manure_mules
assemissions_L<- assheads * Fasses_L * par$prev_asses[i] *par$exc_asses[i]*1000* manure_asses

chickenemissions_L<- chickenheads * Fchickens_L *par$prev_chickens[i] *par$exc_chickens_day[i]*365*1000
duckemissions_L<- duckheads * Fducks_L * par$prev_ducks[i] *par$exc_ducks_day[i]*365*1000

#########  and surviving storage ########
cattleemissions_young_S_survived<- cattleheads * Fcattle_S_survived * par$fraction_young_cattle[i] * prev_cattle_young * par$exc_cattle_young[i]*1000 * manure_cattle_young
cattleemissions_adult_S_survived<- cattleheads * Fcattle_S_survived * (1-par$fraction_young_cattle[i])* prev_cattle_adult * par$exc_cattle_adult[i]*1000 * manure_cattle_adult
cattleemissions_S_survived<- cattleemissions_young_S_survived + cattleemissions_adult_S_survived

buffaloemissions_young_S_survived<- buffaloheads * Fbuffaloes_S_survived * par$fraction_young_buffaloes[i] * par$prev_buffaloes_young[i] * par$exc_buffaloes_young[i]*1000 * manure_buffaloes_young
buffaloemissions_adult_S_survived<- buffaloheads * Fbuffaloes_S_survived * (1-par$fraction_young_buffaloes[i])* par$prev_buffaloes_adult[i] * par$exc_buffaloes_adult[i] *1000* manure_buffaloes_adult
buffaloemissions_S_survived<- buffaloemissions_young_S_survived + buffaloemissions_adult_S_survived

pigemissions_young_S_survived<- pigheads * Fpigs_S_survived * par$fraction_young_pigs[i] * par$prev_pigs_young[i] * par$exc_pigs_young[i]*1000 * manure_pigs_young
pigemissions_adult_S_survived<- pigheads * Fpigs_S_survived * (1-par$fraction_young_pigs[i])* par$prev_pigs_adult[i] * par$exc_pigs_adult[i]*1000 * manure_pigs_adult
pigemissions_S_survived<- pigemissions_young_S_survived + pigemissions_adult_S_survived

goatemissions_young_S_survived<- goatheads * Fgoats_S_survived * par$fraction_young_goats[i] * par$prev_goats_young[i] * par$exc_goats_young[i]*1000 * manure_goats_young
goatemissions_adult_S_survived<- goatheads * Fgoats_S_survived * (1-par$fraction_young_goats[i])* par$prev_goats_adult[i] * par$exc_goats_adult[i]*1000 * manure_goats_adult
goatemissions_S_survived<- goatemissions_young_S_survived + goatemissions_adult_S_survived

sheepemissions_young_S_survived<- sheepheads * Fsheep_S_survived * par$fraction_young_sheep[i] * par$prev_sheep_young[i] * par$exc_sheep_young[i]*1000 * manure_sheep_young
sheepemissions_adult_S_survived<- sheepheads * Fsheep_S_survived * (1-par$fraction_young_sheep[i])* par$prev_sheep_adult[i] * par$exc_sheep_adult[i]*1000 * manure_sheep_adult
sheepemissions_S_survived<- sheepemissions_young_S_survived + sheepemissions_adult_S_survived

horseemissions_S_survived<- horseheads * Fhorses_S_survived * par$prev_horses[i] *par$exc_horses[i]*1000* manure_horses
camelemissions_S_survived<- camelheads * Fcamels_S_survived * par$prev_camels[i] * par$exc_camels[i] *1000* manure_camels
muleemissions_S_survived<- muleheads * Fmules_S_survived * par$prev_mules[i] *par$exc_mules[i] *1000* manure_mules
assemissions_S_survived<- assheads * Fasses_S_survived * par$prev_asses[i] *par$exc_asses[i]*1000* manure_asses

chickenemissions_S_survived<- chickenheads * Fchickens_S_survived *par$prev_chickens[i] *par$exc_chickens_day[i]*365*1000
duckemissions_S_survived<- duckheads * Fducks_S_survived * par$prev_ducks[i] *par$exc_ducks_day[i]*365*1000


### and total going into storage (without accounting for survival) ####


cattleemissions_young_S<- cattleheads * Fcattle_S * par$fraction_young_cattle[i] * prev_cattle_young * par$exc_cattle_young[i]*1000 * manure_cattle_young
cattleemissions_adult_S<- cattleheads * Fcattle_S * (1-par$fraction_young_cattle[i])* prev_cattle_adult * par$exc_cattle_adult[i]*1000 * manure_cattle_adult
cattleemissions_S<- cattleemissions_young_S + cattleemissions_adult_S

buffaloemissions_young_S<- buffaloheads * Fbuffaloes_S * par$fraction_young_buffaloes[i] * par$prev_buffaloes_young[i] * par$exc_buffaloes_young[i]*1000 * manure_buffaloes_young
buffaloemissions_adult_S<- buffaloheads * Fbuffaloes_S * (1-par$fraction_young_buffaloes[i])* par$prev_buffaloes_adult[i] * par$exc_buffaloes_adult[i] *1000* manure_buffaloes_adult
buffaloemissions_S<- buffaloemissions_young_S + buffaloemissions_adult_S

pigemissions_young_S<- pigheads * Fpigs_S * par$fraction_young_pigs[i] * par$prev_pigs_young[i] * par$exc_pigs_young[i]*1000 * manure_pigs_young
pigemissions_adult_S<- pigheads * Fpigs_S * (1-par$fraction_young_pigs[i])* par$prev_pigs_adult[i] * par$exc_pigs_adult[i]*1000 * manure_pigs_adult
pigemissions_S<- pigemissions_young_S + pigemissions_adult_S

goatemissions_young_S<- goatheads * Fgoats_S * par$fraction_young_goats[i] * par$prev_goats_young[i] * par$exc_goats_young[i]*1000 * manure_goats_young
goatemissions_adult_S<- goatheads * Fgoats_S * (1-par$fraction_young_goats[i])* par$prev_goats_adult[i] * par$exc_goats_adult[i]*1000 * manure_goats_adult
goatemissions_S<- goatemissions_young_S + goatemissions_adult_S

sheepemissions_young_S<- sheepheads * Fsheep_S * par$fraction_young_sheep[i] * par$prev_sheep_young[i] * par$exc_sheep_young[i]*1000 * manure_sheep_young
sheepemissions_adult_S<- sheepheads * Fsheep_S * (1-par$fraction_young_sheep[i])* par$prev_sheep_adult[i] * par$exc_sheep_adult[i]*1000 * manure_sheep_adult
sheepemissions_S<- sheepemissions_young_S + sheepemissions_adult_S

horseemissions_S<- horseheads * Fhorses_S * par$prev_horses[i] *par$exc_horses[i]*1000* manure_horses
camelemissions_S<- camelheads * Fcamels_S * par$prev_camels[i] * par$exc_camels[i] *1000* manure_camels
muleemissions_S<- muleheads * Fmules_S * par$prev_mules[i] *par$exc_mules[i] *1000* manure_mules
assemissions_S<- assheads * Fasses_S * par$prev_asses[i] *par$exc_asses[i]*1000* manure_asses

chickenemissions_S<- chickenheads * Fchickens_S *par$prev_chickens[i] *par$exc_chickens_day[i]*365*1000
duckemissions_S<- duckheads * Fducks_S * par$prev_ducks[i] *par$exc_ducks_day[i]*365*1000

########## compute total diffuse emission raster and log version #############
totaldiffuse<-sum(cattleemissions, chickenemissions,duckemissions,goatemissions,sheepemissions,pigemissions,buffaloemissions,horseemissions,assemissions,camelemissions,muleemissions,na.rm=TRUE)
totaldiffuselog<-log10(totaldiffuse)
#plot(totaldiffuse,main= "Diffuse oocyst emissions")
#plot(totaldiffuselog, main = "Diffuse oocyst emissions (log)")
#plot(totaldiffuselog, col=colors, main= "Diffuse oocyst emissions (log)")
#plot(totaldiffuselog, col=colors2, main= paste0("Diffuse oocyst emissions (log)", i)) 

totaldiffuse_L<-sum(cattleemissions_L, chickenemissions_L,duckemissions_L,goatemissions_L,sheepemissions_L,pigemissions_L,buffaloemissions_L,horseemissions_L,assemissions_L,camelemissions_L,muleemissions_L,na.rm=TRUE)
totaldiffuse_S<-sum(cattleemissions_S, chickenemissions_S,duckemissions_S,goatemissions_S,sheepemissions_S,pigemissions_S,buffaloemissions_S,horseemissions_S,assemissions_S,camelemissions_S,muleemissions_S,na.rm=TRUE)
totaldiffuse_S_survived<-sum(cattleemissions_S_survived, chickenemissions_S_survived,duckemissions_S_survived,goatemissions_S_survived,sheepemissions_S_survived,pigemissions_S_survived,buffaloemissions_S_survived,horseemissions_S_survived,assemissions_S_survived,camelemissions_S_survived,muleemissions_S_survived,na.rm=TRUE)

####### write rasters #############
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_total",i, ".asc")
writeRaster(totaldiffuse,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_total_L",i, ".asc")
writeRaster(totaldiffuse_L,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_total_S",i, ".asc")
writeRaster(totaldiffuse_S,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_total_S_survived",i, ".asc")
writeRaster(totaldiffuse_S_survived,filename=filename,format="ascii", overwrite=TRUE)

filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_cattle",i, ".asc")
writeRaster(cattleemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_cattle_young",i, ".asc")
writeRaster(cattleemissions_young,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_cattle_adult",i, ".asc")
writeRaster(cattleemissions_adult,filename=filename,format="ascii", overwrite=TRUE)

filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_buffaloes",i, ".asc")
writeRaster(buffaloemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_buffaloes_young",i, ".asc")
writeRaster(buffaloemissions_young,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_buffaloes_adult",i, ".asc")
writeRaster(buffaloemissions_adult,filename=filename,format="ascii", overwrite=TRUE)

filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_goats",i, ".asc")
writeRaster(goatemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_goats_young",i, ".asc")
writeRaster(goatemissions_young,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_goats_adult",i, ".asc")
writeRaster(goatemissions_adult,filename=filename,format="ascii", overwrite=TRUE)


filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_sheep",i, ".asc")
writeRaster(sheepemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_sheep_young",i, ".asc")
writeRaster(sheepemissions_young,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_sheep_adult",i, ".asc")
writeRaster(sheepemissions_adult,filename=filename,format="ascii", overwrite=TRUE)

filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_pigs",i, ".asc")
writeRaster(pigemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_pigs_young",i, ".asc")
writeRaster(pigemissions_young,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_pigs_adult",i, ".asc")
writeRaster(pigemissions_adult,filename=filename,format="ascii", overwrite=TRUE)

filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_chickens",i, ".asc")
writeRaster(chickenemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_ducks",i, ".asc")
writeRaster(duckemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_horses",i, ".asc")
writeRaster(horseemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_asses",i, ".asc")
writeRaster(assemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_camels",i, ".asc")
writeRaster(camelemissions,filename=filename,format="ascii", overwrite=TRUE)
filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_emissions_mules",i, ".asc")
writeRaster(muleemissions,filename=filename,format="ascii", overwrite=TRUE)

# add a nice plot export function here for the log diffuse emissions map

######### source attribution #############
cattletotal<-cellStats(cattleemissions, stat='sum', na.rm=TRUE)
cattleyoung<-cellStats(cattleemissions_young, stat='sum', na.rm=TRUE)
cattleadult<-cellStats(cattleemissions_adult, stat='sum', na.rm=TRUE)

buffalototal<-cellStats(buffaloemissions, stat='sum', na.rm=TRUE)
buffaloyoung<-cellStats(buffaloemissions_young, stat='sum', na.rm=TRUE)
buffaloadult<-cellStats(buffaloemissions_adult, stat='sum', na.rm=TRUE)

goattotal<-cellStats(goatemissions, stat='sum',na.rm=TRUE)
goatyoung<-cellStats(goatemissions_young, stat='sum', na.rm=TRUE)
goatadult<-cellStats(goatemissions_adult, stat='sum', na.rm=TRUE)

sheeptotal<-cellStats(sheepemissions, stat='sum', na.rm=TRUE)
sheepyoung<-cellStats(sheepemissions_young, stat='sum', na.rm=TRUE)
sheepadult<-cellStats(sheepemissions_adult, stat='sum', na.rm=TRUE)

pigtotal<-cellStats(pigemissions, stat='sum', na.rm=TRUE)
pigyoung<-cellStats(pigemissions_young, stat='sum', na.rm=TRUE)
pigadult<-cellStats(pigemissions_adult, stat='sum', na.rm=TRUE)

chickentotal<-cellStats(chickenemissions, stat='sum', na.rm=TRUE)
ducktotal<-cellStats(duckemissions, stat='sum', na.rm=TRUE)
horsetotal<-cellStats(horseemissions, stat='sum',na.rm=TRUE)
asstotal<-cellStats(assemissions, stat='sum', na.rm=TRUE)
cameltotal<-cellStats(camelemissions, stat='sum', na.rm=TRUE)
muletotal<-cellStats(muleemissions, stat='sum', na.rm=TRUE)

##### filling the results dataframe 
results$cattletotal[i]<-cattletotal
results$cattleyoung[i]<-cattleyoung
results$cattleadult[i]<-cattleadult
results$buffalototal[i]<-buffalototal
results$buffaloyoung[i]<-buffaloyoung
results$buffaloadult[i]<-buffaloadult
results$goattotal[i]<-goattotal
results$goatyoung[i]<-goatyoung
results$goatadult[i]<-goatadult
results$sheeptotal[i]<-sheeptotal
results$sheepyoung[i]<-sheepyoung
results$sheepadult[i]<-sheepadult
results$pigtotal[i]<-pigtotal
results$pigyoung[i]<-pigyoung
results$pigadult[i]<-pigadult
results$chickentotal[i]<-chickentotal
results$ducktotal[i]<-ducktotal
results$horsetotal[i]<-horsetotal
results$asstotal[i]<-asstotal
results$cameltotal[i]<-cameltotal
results$muletotal[i]<-muletotal

results$cattletotal_log[i]<-log10(cattletotal)
results$cattleyoung_log[i]<-log10(cattleyoung)
results$cattleadult_log[i]<-log10(cattleadult)
results$buffalototal_log[i]<-log10(buffalototal)
results$buffaloyoung_log[i]<-log10(buffaloyoung)
results$buffaloadult_log[i]<-log10(buffaloadult)
results$goattotal_log[i]<-log10(goattotal)
results$goatyoung_log[i]<-log10(goatyoung)
results$goatadult_log[i]<-log10(goatadult)
results$sheeptotal_log[i]<-log10(sheeptotal)
results$sheepyoung_log[i]<-log10(sheepyoung)
results$sheepadult_log[i]<-log10(sheepadult)
results$pigtotal_log[i]<-log10(pigtotal)
results$pigyoung_log[i]<-log10(pigyoung)
results$pigadult_log[i]<-log10(pigadult)
results$chickentotal_log[i]<-log10(chickentotal)
results$ducktotal_log[i]<-log10(ducktotal)
results$horsetotal_log[i]<-log10(horsetotal)
results$asstotal_log[i]<-log10(asstotal)
results$cameltotal_log[i]<-log10(cameltotal)
results$muletotal_log[i]<-log10(muletotal)

######### source attribution to land #############

cattletotal_L<-cellStats(cattleemissions_L, stat='sum', na.rm=TRUE)
cattleyoung_L<-cellStats(cattleemissions_young_L, stat='sum', na.rm=TRUE)
cattleadult_L<-cellStats(cattleemissions_adult_L, stat='sum', na.rm=TRUE)

buffalototal_L<-cellStats(buffaloemissions_L, stat='sum', na.rm=TRUE)
buffaloyoung_L<-cellStats(buffaloemissions_young_L, stat='sum', na.rm=TRUE)
buffaloadult_L<-cellStats(buffaloemissions_adult_L, stat='sum', na.rm=TRUE)

goattotal_L<-cellStats(goatemissions_L, stat='sum',na.rm=TRUE)
goatyoung_L<-cellStats(goatemissions_young_L, stat='sum', na.rm=TRUE)
goatadult_L<-cellStats(goatemissions_adult_L, stat='sum', na.rm=TRUE)

sheeptotal_L<-cellStats(sheepemissions_L, stat='sum', na.rm=TRUE)
sheepyoung_L<-cellStats(sheepemissions_young_L, stat='sum', na.rm=TRUE)
sheepadult_L<-cellStats(sheepemissions_adult_L, stat='sum', na.rm=TRUE)

pigtotal_L<-cellStats(pigemissions_L, stat='sum', na.rm=TRUE)
pigyoung_L<-cellStats(pigemissions_young_L, stat='sum', na.rm=TRUE)
pigadult_L<-cellStats(pigemissions_adult_L, stat='sum', na.rm=TRUE)

chickentotal_L<-cellStats(chickenemissions_L, stat='sum', na.rm=TRUE)
ducktotal_L<-cellStats(duckemissions_L, stat='sum', na.rm=TRUE)
horsetotal_L<-cellStats(horseemissions_L, stat='sum',na.rm=TRUE)
asstotal_L<-cellStats(assemissions_L, stat='sum', na.rm=TRUE)
cameltotal_L<-cellStats(camelemissions_L, stat='sum', na.rm=TRUE)
muletotal_L<-cellStats(muleemissions_L, stat='sum', na.rm=TRUE)

#filling the results dataframe
results$cattletotal_L[i]<-cattletotal_L
results$cattleyoung_L[i]<-cattleyoung_L
results$cattleadult_L[i]<-cattleadult_L
results$buffalototal_L[i]<-buffalototal_L
results$buffaloyoung_L[i]<-buffaloyoung_L
results$buffaloadult_L[i]<-buffaloadult_L
results$goattotal_L[i]<-goattotal_L
results$goatyoung_L[i]<-goatyoung_L
results$goatadult_L[i]<-goatadult_L
results$sheeptotal_L[i]<-sheeptotal_L
results$sheepyoung_L[i]<-sheepyoung_L
results$sheepadult_L[i]<-sheepadult_L
results$pigtotal_L[i]<-pigtotal_L
results$pigyoung_L[i]<-pigyoung_L
results$pigadult_L[i]<-pigadult_L
results$chickentotal_L[i]<-chickentotal_L
results$ducktotal_L[i]<-ducktotal_L
results$horsetotal_L[i]<-horsetotal_L
results$asstotal_L[i]<-asstotal_L
results$cameltotal_L[i]<-cameltotal_L
results$muletotal_L[i]<-muletotal_L

######### source attribution to storage #############

cattletotal_S<-cellStats(cattleemissions_S, stat='sum', na.rm=TRUE)
cattleyoung_S<-cellStats(cattleemissions_young_S, stat='sum', na.rm=TRUE)
cattleadult_S<-cellStats(cattleemissions_adult_S, stat='sum', na.rm=TRUE)

buffalototal_S<-cellStats(buffaloemissions_S, stat='sum', na.rm=TRUE)
buffaloyoung_S<-cellStats(buffaloemissions_young_S, stat='sum', na.rm=TRUE)
buffaloadult_S<-cellStats(buffaloemissions_adult_S, stat='sum', na.rm=TRUE)

goattotal_S<-cellStats(goatemissions_S, stat='sum',na.rm=TRUE)
goatyoung_S<-cellStats(goatemissions_young_S, stat='sum', na.rm=TRUE)
goatadult_S<-cellStats(goatemissions_adult_S, stat='sum', na.rm=TRUE)

sheeptotal_S<-cellStats(sheepemissions_S, stat='sum', na.rm=TRUE)
sheepyoung_S<-cellStats(sheepemissions_young_S, stat='sum', na.rm=TRUE)
sheepadult_S<-cellStats(sheepemissions_adult_S, stat='sum', na.rm=TRUE)

pigtotal_S<-cellStats(pigemissions_S, stat='sum', na.rm=TRUE)
pigyoung_S<-cellStats(pigemissions_young_S, stat='sum', na.rm=TRUE)
pigadult_S<-cellStats(pigemissions_adult_S, stat='sum', na.rm=TRUE)

chickentotal_S<-cellStats(chickenemissions_S, stat='sum', na.rm=TRUE)
ducktotal_S<-cellStats(duckemissions_S, stat='sum', na.rm=TRUE)
horsetotal_S<-cellStats(horseemissions_S, stat='sum',na.rm=TRUE)
asstotal_S<-cellStats(assemissions_S, stat='sum', na.rm=TRUE)
cameltotal_S<-cellStats(camelemissions_S, stat='sum', na.rm=TRUE)
muletotal_S<-cellStats(muleemissions_S, stat='sum', na.rm=TRUE)

#filling the results dataframe
results$cattletotal_S[i]<-cattletotal_S
results$cattleyoung_S[i]<-cattleyoung_S
results$cattleadult_S[i]<-cattleadult_S
results$buffalototal_S[i]<-buffalototal_S
results$buffaloyoung_S[i]<-buffaloyoung_S
results$buffaloadult_S[i]<-buffaloadult_S
results$goattotal_S[i]<-goattotal_S
results$goatyoung_S[i]<-goatyoung_S
results$goatadult_S[i]<-goatadult_S
results$sheeptotal_S[i]<-sheeptotal_S
results$sheepyoung_S[i]<-sheepyoung_S
results$sheepadult_S[i]<-sheepadult_S
results$pigtotal_S[i]<-pigtotal_S
results$pigyoung_S[i]<-pigyoung_S
results$pigadult_S[i]<-pigadult_S
results$chickentotal_S[i]<-chickentotal_S
results$ducktotal_S[i]<-ducktotal_S
results$horsetotal_S[i]<-horsetotal_S
results$asstotal_S[i]<-asstotal_S
results$cameltotal_S[i]<-cameltotal_S
results$muletotal_S[i]<-muletotal_S

######### source attribution to storage accounting for what survived #############

cattletotal_S_survived<-cellStats(cattleemissions_S_survived, stat='sum', na.rm=TRUE)
cattleyoung_S_survived<-cellStats(cattleemissions_young_S_survived, stat='sum', na.rm=TRUE)
cattleadult_S_survived<-cellStats(cattleemissions_adult_S_survived, stat='sum', na.rm=TRUE)

buffalototal_S_survived<-cellStats(buffaloemissions_S_survived, stat='sum', na.rm=TRUE)
buffaloyoung_S_survived<-cellStats(buffaloemissions_young_S_survived, stat='sum', na.rm=TRUE)
buffaloadult_S_survived<-cellStats(buffaloemissions_adult_S_survived, stat='sum', na.rm=TRUE)

goattotal_S_survived<-cellStats(goatemissions_S_survived, stat='sum',na.rm=TRUE)
goatyoung_S_survived<-cellStats(goatemissions_young_S_survived, stat='sum', na.rm=TRUE)
goatadult_S_survived<-cellStats(goatemissions_adult_S_survived, stat='sum', na.rm=TRUE)

sheeptotal_S_survived<-cellStats(sheepemissions_S_survived, stat='sum', na.rm=TRUE)
sheepyoung_S_survived<-cellStats(sheepemissions_young_S_survived, stat='sum', na.rm=TRUE)
sheepadult_S_survived<-cellStats(sheepemissions_adult_S_survived, stat='sum', na.rm=TRUE)

pigtotal_S_survived<-cellStats(pigemissions_S_survived, stat='sum', na.rm=TRUE)
pigyoung_S_survived<-cellStats(pigemissions_young_S_survived, stat='sum', na.rm=TRUE)
pigadult_S_survived<-cellStats(pigemissions_adult_S_survived, stat='sum', na.rm=TRUE)

chickentotal_S_survived<-cellStats(chickenemissions_S_survived, stat='sum', na.rm=TRUE)
ducktotal_S_survived<-cellStats(duckemissions_S_survived, stat='sum', na.rm=TRUE)
horsetotal_S_survived<-cellStats(horseemissions_S_survived, stat='sum',na.rm=TRUE)
asstotal_S_survived<-cellStats(assemissions_S_survived, stat='sum', na.rm=TRUE)
cameltotal_S_survived<-cellStats(camelemissions_S_survived, stat='sum', na.rm=TRUE)
muletotal_S_survived<-cellStats(muleemissions_S_survived, stat='sum', na.rm=TRUE)

#filling the results dataframe
results$cattletotal_S_survived[i]<-cattletotal_S_survived
results$cattleyoung_S_survived[i]<-cattleyoung_S_survived
results$cattleadult_S_survived[i]<-cattleadult_S_survived
results$buffalototal_S_survived[i]<-buffalototal_S_survived
results$buffaloyoung_S_survived[i]<-buffaloyoung_S_survived
results$buffaloadult_S_survived[i]<-buffaloadult_S_survived
results$goattotal_S_survived[i]<-goattotal_S_survived
results$goatyoung_S_survived[i]<-goatyoung_S_survived
results$goatadult_S_survived[i]<-goatadult_S_survived
results$sheeptotal_S_survived[i]<-sheeptotal_S_survived
results$sheepyoung_S_survived[i]<-sheepyoung_S_survived
results$sheepadult_S_survived[i]<-sheepadult_S_survived
results$pigtotal_S_survived[i]<-pigtotal_S_survived
results$pigyoung_S_survived[i]<-pigyoung_S_survived
results$pigadult_S_survived[i]<-pigadult_S_survived
results$chickentotal_S_survived[i]<-chickentotal_S_survived
results$ducktotal_S_survived[i]<-ducktotal_S_survived
results$horsetotal_S_survived[i]<-horsetotal_S_survived
results$asstotal_S_survived[i]<-asstotal_S_survived
results$cameltotal_S_survived[i]<-cameltotal_S_survived
results$muletotal_S_survived[i]<-muletotal_S_survived

#get all in data frame
#names<-c("cattletotal","cattleyoung","cattleadult","buffalototal","buffaloyoung","buffaloadult","goattotal","goatyoung","goatadult","sheeptotal","sheepyoung","sheepadult","pigtotal","pigyoung","pigadult","chickentotal","ducktotal","horsetotal","asstotal","cameltotal","muletotal")
#values<-c(cattletotal, cattleyoung, cattleadult,buffalototal,buffaloyoung,buffaloadult,goattotal,goatyoung,goatadult,sheeptotal,sheepyoung,sheepadult,pigtotal,pigyoung,pigadult,chickentotal,ducktotal,horsetotal,asstotal,cameltotal,muletotal)
#totaldata<-data.frame(names,values)
#totaldata$log<-log10(totaldata$values)
#sum(totaldata$values)
#barplot(totaldata$values)
#barplot(totaldata$log)
#filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_sourceatrribution",i, ".csv")
#write.csv(totaldata,file=filename) #, overwrite=TRUE

######## source attribution to intensive extensive ##########

# first, create rasters representing intensive/extensive division
# account for the part surviving storage
#then calculate what of the final result should be attributed to intensive
# 1- this raster will be what can be attributed to extensive
#Fcattle_E_surv + Fcattle_I_surv = Fcattle_toland

Fcattle_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_SE.asc")*Fcattle_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_LE.asc")
Fcattle_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_SI.asc")*Fcattle_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_LI.asc")
Fcattle_I_source<-Fcattle_I_surv/(Fcattle_I_surv+Fcattle_E_surv)
cattleemissions_I<-cattleemissions*Fcattle_I_source

Fbuffaloes_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_SE.asc")*Fbuffaloes_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_LE.asc")
Fbuffaloes_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_SI.asc")*Fbuffaloes_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_LI.asc")
Fbuffaloes_I_source<-Fbuffaloes_I_surv/(Fbuffaloes_I_surv+Fbuffaloes_E_surv)
buffaloemissions_I<-buffaloemissions*Fbuffaloes_I_source

Fpigs_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_SE.asc")*Fpigs_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_LE.asc")
Fpigs_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_SI.asc")*Fpigs_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_LI.asc")
Fpigs_I_source<-Fpigs_I_surv/(Fpigs_I_surv+Fpigs_E_surv)
pigemissions_I<-pigemissions*Fpigs_I_source

Fsheep_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_SE.asc")*Fsheep_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_LE.asc")
Fsheep_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_SI.asc")*Fsheep_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_LI.asc")
Fsheep_I_source<-Fsheep_I_surv/(Fsheep_I_surv+Fsheep_E_surv)
sheepemissions_I<-sheepemissions*Fsheep_I_source

Fgoats_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_SE.asc")*Fgoats_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_LE.asc")
Fgoats_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_SI.asc")*Fgoats_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_LI.asc")
Fgoats_I_source<-Fgoats_I_surv/(Fgoats_I_surv+Fgoats_E_surv)
goatemissions_I<-goatemissions*Fgoats_I_source

Fchickens_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_SE.asc")*Fpoultry_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_LE.asc")
Fchickens_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_SI.asc")*Fpoultry_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fchickens_LI.asc")
Fchickens_I_source<-Fchickens_I_surv/(Fchickens_I_surv+Fchickens_E_surv)
chickenemissions_I<-chickenemissions*Fchickens_I_source

Fducks_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_SE.asc")*Fpoultry_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_LE.asc")
Fducks_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_SI.asc")*Fpoultry_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fducks_LI.asc")
Fducks_I_source<-Fducks_I_surv/(Fducks_I_surv+Fducks_E_surv)
duckemissions_I<-duckemissions*Fducks_I_source

Fhorses_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_SE.asc")*Fhorses_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_LE.asc")
Fhorses_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_SI.asc")*Fhorses_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_LI.asc")
Fhorses_I_source<-Fhorses_I_surv/(Fhorses_I_surv+Fhorses_E_surv)
horseemissions_I<-horseemissions*Fhorses_I_source

Fcamels_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_SE.asc")*Fcamels_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_LE.asc")
Fcamels_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_SI.asc")*Fcamels_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_LI.asc")
Fcamels_I_source<-Fcamels_I_surv/(Fcamels_I_surv+Fcamels_E_surv)
camelemissions_I<-camelemissions*Fcamels_I_source

Fmules_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_SE.asc")*Fmules_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_LE.asc")
Fmules_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_SI.asc")*Fmules_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_LI.asc")
Fmules_I_source<-Fmules_I_surv/(Fmules_I_surv+Fmules_E_surv)
muleemissions_I<-muleemissions*Fmules_I_source

Fasses_E_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_SE.asc")*Fasses_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_LE.asc")
Fasses_I_surv<-raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_SI.asc")*Fasses_survivingstorage+raster("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_LI.asc")
Fasses_I_source<-Fasses_I_surv/(Fasses_I_surv+Fasses_E_surv)
assemissions_I<-assemissions*Fasses_I_source

cattletotal_I<-cellStats(cattleemissions_I, stat='sum', na.rm=TRUE)
buffalototal_I<-cellStats(buffaloemissions_I, stat='sum', na.rm=TRUE)
goattotal_I<-cellStats(goatemissions_I, stat='sum',na.rm=TRUE)
sheeptotal_I<-cellStats(sheepemissions_I, stat='sum', na.rm=TRUE)
pigtotal_I<-cellStats(pigemissions_I, stat='sum', na.rm=TRUE)
chickentotal_I<-cellStats(chickenemissions_I, stat='sum', na.rm=TRUE)
ducktotal_I<-cellStats(duckemissions_I, stat='sum', na.rm=TRUE)
horsetotal_I<-cellStats(horseemissions_I, stat='sum',na.rm=TRUE)
asstotal_I<-cellStats(assemissions_I, stat='sum', na.rm=TRUE)
cameltotal_I<-cellStats(camelemissions_I, stat='sum', na.rm=TRUE)
muletotal_I<-cellStats(muleemissions_I, stat='sum', na.rm=TRUE)

#filling the results dataframe
results$cattletotal_I[i]<-cattletotal_I
results$buffalototal_I[i]<-buffalototal_I
results$goattotal_I[i]<-goattotal_I
results$sheeptotal_I[i]<-sheeptotal_I
results$pigtotal_I[i]<-pigtotal_I
results$chickentotal_I[i]<-chickentotal_I
results$ducktotal_I[i]<-ducktotal_I
results$horsetotal_I[i]<-horsetotal_I
results$asstotal_I[i]<-asstotal_I
results$cameltotal_I[i]<-cameltotal_I
results$muletotal_I[i]<-muletotal_I

####### regional attribution #########

#regional source attribution
#reading in region raster
# codes in this file are:
# Africa = 1
# Asia = 2
# Europe = 3
# Latin America = 4
# Middle East North Africa = 5
# North America = 6
# Oceania = 7
africa<-regionraster == 1
asia<-regionraster==2
europe<-regionraster==3
latinamerica<-regionraster==4
mena<-regionraster==5
northamerica<-regionraster==6
oceania<-regionraster==7

results$africatotal[i]<-sum(totaldiffuse[africa], na.rm=TRUE)
results$asiatotal[i]<-sum(totaldiffuse[asia], na.rm=TRUE)
results$europetotal[i]<-sum(totaldiffuse[europe], na.rm=TRUE)
results$latinamericatotal[i]<-sum(totaldiffuse[latinamerica], na.rm=TRUE)
results$menatotal[i]<-sum(totaldiffuse[mena], na.rm=TRUE)
results$northamericatotal[i]<-sum(totaldiffuse[northamerica], na.rm=TRUE)
results$oceaniatotal[i]<-sum(totaldiffuse[oceania], na.rm=TRUE)

results$africatotal_L[i]<-sum(totaldiffuse_L[africa], na.rm=TRUE)
results$asiatotal_L[i]<-sum(totaldiffuse_L[asia], na.rm=TRUE)
results$europetotal_L[i]<-sum(totaldiffuse_L[europe], na.rm=TRUE)
results$latinamericatotal_L[i]<-sum(totaldiffuse_L[latinamerica], na.rm=TRUE)
results$menatotal_L[i]<-sum(totaldiffuse_L[mena], na.rm=TRUE)
results$northamericatotal_L[i]<-sum(totaldiffuse_L[northamerica], na.rm=TRUE)
results$oceaniatotal_L[i]<-sum(totaldiffuse_L[oceania], na.rm=TRUE)

results$africatotal_S[i]<-sum(totaldiffuse_S[africa], na.rm=TRUE)
results$asiatotal_S[i]<-sum(totaldiffuse_S[asia], na.rm=TRUE)
results$europetotal_S[i]<-sum(totaldiffuse_S[europe], na.rm=TRUE)
results$latinamericatotal_S[i]<-sum(totaldiffuse_S[latinamerica], na.rm=TRUE)
results$menatotal_S[i]<-sum(totaldiffuse_S[mena], na.rm=TRUE)
results$northamericatotal_S[i]<-sum(totaldiffuse_S[northamerica], na.rm=TRUE)
results$oceaniatotal_S[i]<-sum(totaldiffuse_S[oceania], na.rm=TRUE)

results$africatotal_S_survived[i]<-sum(totaldiffuse_S_survived[africa], na.rm=TRUE)
results$asiatotal_S_survived[i]<-sum(totaldiffuse_S_survived[asia], na.rm=TRUE)
results$europetotal_S_survived[i]<-sum(totaldiffuse_S_survived[europe], na.rm=TRUE)
results$latinamericatotal_S_survived[i]<-sum(totaldiffuse_S_survived[latinamerica], na.rm=TRUE)
results$menatotal_S_survived[i]<-sum(totaldiffuse_S_survived[mena], na.rm=TRUE)
results$northamericatotal_S_survived[i]<-sum(totaldiffuse_S_survived[northamerica], na.rm=TRUE)
results$oceaniatotal_S_survived[i]<-sum(totaldiffuse_S_survived[oceania], na.rm=TRUE)


##### separate regional table ######

regions<-c(rep("africa",12),rep("asia",12), rep("europe", 12),rep("latinamerica",12), rep("mena",12), rep("northamerica", 12), rep("oceania",12))
animalnames<-c("cattle", "buffaloes", "goats", "sheep", "pigs","chickens", "ducks", "horses", "camels", "mules", "asses", "total")
regionsources<-data.frame(regions, animalnames)

regionsources$emissions<-NA
regionsources$emissions[1]<-sum(cattleemissions[africa], na.rm=TRUE)
regionsources$emissions[2]<-sum(buffaloemissions[africa], na.rm=TRUE)
regionsources$emissions[3]<-sum(goatemissions[africa], na.rm=TRUE)
regionsources$emissions[4]<-sum(sheepemissions[africa], na.rm=TRUE)
regionsources$emissions[5]<-sum(pigemissions[africa], na.rm=TRUE)
regionsources$emissions[6]<-sum(chickenemissions[africa], na.rm=TRUE)
regionsources$emissions[7]<-sum(duckemissions[africa], na.rm=TRUE)
regionsources$emissions[8]<-sum(horseemissions[africa], na.rm=TRUE)
regionsources$emissions[9]<-sum(camelemissions[africa], na.rm=TRUE)
regionsources$emissions[10]<-sum(muleemissions[africa], na.rm=TRUE)
regionsources$emissions[11]<-sum(assemissions[africa], na.rm=TRUE)
regionsources$emissions[12]<-sum(totaldiffuse[africa], na.rm=TRUE)

regionsources$emissions[13]<-sum(cattleemissions[asia], na.rm=TRUE)
regionsources$emissions[14]<-sum(buffaloemissions[asia], na.rm=TRUE)
regionsources$emissions[15]<-sum(goatemissions[asia], na.rm=TRUE)
regionsources$emissions[16]<-sum(sheepemissions[asia], na.rm=TRUE)
regionsources$emissions[17]<-sum(pigemissions[asia], na.rm=TRUE)
regionsources$emissions[18]<-sum(chickenemissions[asia], na.rm=TRUE)
regionsources$emissions[19]<-sum(duckemissions[asia], na.rm=TRUE)
regionsources$emissions[20]<-sum(horseemissions[asia], na.rm=TRUE)
regionsources$emissions[21]<-sum(camelemissions[asia], na.rm=TRUE)
regionsources$emissions[22]<-sum(muleemissions[asia], na.rm=TRUE)
regionsources$emissions[23]<-sum(assemissions[asia], na.rm=TRUE)
regionsources$emissions[24]<-sum(totaldiffuse[asia], na.rm=TRUE)

regionsources$emissions[25]<-sum(cattleemissions[europe], na.rm=TRUE)
regionsources$emissions[26]<-sum(buffaloemissions[europe], na.rm=TRUE)
regionsources$emissions[27]<-sum(goatemissions[europe], na.rm=TRUE)
regionsources$emissions[28]<-sum(sheepemissions[europe], na.rm=TRUE)
regionsources$emissions[29]<-sum(pigemissions[europe], na.rm=TRUE)
regionsources$emissions[30]<-sum(chickenemissions[europe], na.rm=TRUE)
regionsources$emissions[31]<-sum(duckemissions[europe], na.rm=TRUE)
regionsources$emissions[32]<-sum(horseemissions[europe], na.rm=TRUE)
regionsources$emissions[33]<-sum(camelemissions[europe], na.rm=TRUE)
regionsources$emissions[34]<-sum(muleemissions[europe], na.rm=TRUE)
regionsources$emissions[35]<-sum(assemissions[europe], na.rm=TRUE)
regionsources$emissions[36]<-sum(totaldiffuse[europe], na.rm=TRUE)

regionsources$emissions[37]<-sum(cattleemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[38]<-sum(buffaloemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[39]<-sum(goatemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[40]<-sum(sheepemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[41]<-sum(pigemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[42]<-sum(chickenemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[43]<-sum(duckemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[44]<-sum(horseemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[45]<-sum(camelemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[46]<-sum(muleemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[47]<-sum(assemissions[latinamerica], na.rm=TRUE)
regionsources$emissions[48]<-sum(totaldiffuse[latinamerica], na.rm=TRUE)

regionsources$emissions[49]<-sum(cattleemissions[mena], na.rm=TRUE)
regionsources$emissions[50]<-sum(buffaloemissions[mena], na.rm=TRUE)
regionsources$emissions[51]<-sum(goatemissions[mena], na.rm=TRUE)
regionsources$emissions[52]<-sum(sheepemissions[mena], na.rm=TRUE)
regionsources$emissions[53]<-sum(pigemissions[mena], na.rm=TRUE)
regionsources$emissions[54]<-sum(chickenemissions[mena], na.rm=TRUE)
regionsources$emissions[55]<-sum(duckemissions[mena], na.rm=TRUE)
regionsources$emissions[56]<-sum(horseemissions[mena], na.rm=TRUE)
regionsources$emissions[57]<-sum(camelemissions[mena], na.rm=TRUE)
regionsources$emissions[58]<-sum(muleemissions[mena], na.rm=TRUE)
regionsources$emissions[59]<-sum(assemissions[mena], na.rm=TRUE)
regionsources$emissions[60]<-sum(totaldiffuse[mena], na.rm=TRUE)

regionsources$emissions[61]<-sum(cattleemissions[northamerica], na.rm=TRUE)
regionsources$emissions[62]<-sum(buffaloemissions[northamerica], na.rm=TRUE)
regionsources$emissions[63]<-sum(goatemissions[northamerica], na.rm=TRUE)
regionsources$emissions[64]<-sum(sheepemissions[northamerica], na.rm=TRUE)
regionsources$emissions[65]<-sum(pigemissions[northamerica], na.rm=TRUE)
regionsources$emissions[66]<-sum(chickenemissions[northamerica], na.rm=TRUE)
regionsources$emissions[67]<-sum(duckemissions[northamerica], na.rm=TRUE)
regionsources$emissions[68]<-sum(horseemissions[northamerica], na.rm=TRUE)
regionsources$emissions[69]<-sum(camelemissions[northamerica], na.rm=TRUE)
regionsources$emissions[70]<-sum(muleemissions[northamerica], na.rm=TRUE)
regionsources$emissions[71]<-sum(assemissions[northamerica], na.rm=TRUE)
regionsources$emissions[72]<-sum(totaldiffuse[northamerica], na.rm=TRUE)

regionsources$emissions[73]<-sum(cattleemissions[oceania], na.rm=TRUE)
regionsources$emissions[74]<-sum(buffaloemissions[oceania], na.rm=TRUE)
regionsources$emissions[75]<-sum(goatemissions[oceania], na.rm=TRUE)
regionsources$emissions[76]<-sum(sheepemissions[oceania], na.rm=TRUE)
regionsources$emissions[77]<-sum(pigemissions[oceania], na.rm=TRUE)
regionsources$emissions[78]<-sum(chickenemissions[oceania], na.rm=TRUE)
regionsources$emissions[79]<-sum(duckemissions[oceania], na.rm=TRUE)
regionsources$emissions[80]<-sum(horseemissions[oceania], na.rm=TRUE)
regionsources$emissions[81]<-sum(camelemissions[oceania], na.rm=TRUE)
regionsources$emissions[82]<-sum(muleemissions[oceania], na.rm=TRUE)
regionsources$emissions[83]<-sum(assemissions[oceania], na.rm=TRUE)
regionsources$emissions[84]<-sum(totaldiffuse[oceania], na.rm=TRUE)

######### also add heads #############

regionsources$heads<-NA
regionsources$heads[1]<-sum(cattleheads[africa], na.rm=TRUE)
regionsources$heads[2]<-sum(buffaloheads[africa], na.rm=TRUE)
regionsources$heads[3]<-sum(goatheads[africa], na.rm=TRUE)
regionsources$heads[4]<-sum(sheepheads[africa], na.rm=TRUE)
regionsources$heads[5]<-sum(pigheads[africa], na.rm=TRUE)
regionsources$heads[6]<-sum(chickenheads[africa], na.rm=TRUE)
regionsources$heads[7]<-sum(duckheads[africa], na.rm=TRUE)
regionsources$heads[8]<-sum(horseheads[africa], na.rm=TRUE)
regionsources$heads[9]<-sum(camelheads[africa], na.rm=TRUE)
regionsources$heads[10]<-sum(muleheads[africa], na.rm=TRUE)
regionsources$heads[11]<-sum(assheads[africa], na.rm=TRUE)
regionsources$heads[12]<-sum(regionsources$heads[c(1:11)], na.rm=TRUE)

regionsources$heads[13]<-sum(cattleheads[asia], na.rm=TRUE)
regionsources$heads[14]<-sum(buffaloheads[asia], na.rm=TRUE)
regionsources$heads[15]<-sum(goatheads[asia], na.rm=TRUE)
regionsources$heads[16]<-sum(sheepheads[asia], na.rm=TRUE)
regionsources$heads[17]<-sum(pigheads[asia], na.rm=TRUE)
regionsources$heads[18]<-sum(chickenheads[asia], na.rm=TRUE)
regionsources$heads[19]<-sum(duckheads[asia], na.rm=TRUE)
regionsources$heads[20]<-sum(horseheads[asia], na.rm=TRUE)
regionsources$heads[21]<-sum(camelheads[asia], na.rm=TRUE)
regionsources$heads[22]<-sum(muleheads[asia], na.rm=TRUE)
regionsources$heads[23]<-sum(assheads[asia], na.rm=TRUE)
regionsources$heads[24]<-sum(regionsources$heads[c(13:23)], na.rm=TRUE)

regionsources$heads[25]<-sum(cattleheads[europe], na.rm=TRUE)
regionsources$heads[26]<-sum(buffaloheads[europe], na.rm=TRUE)
regionsources$heads[27]<-sum(goatheads[europe], na.rm=TRUE)
regionsources$heads[28]<-sum(sheepheads[europe], na.rm=TRUE)
regionsources$heads[29]<-sum(pigheads[europe], na.rm=TRUE)
regionsources$heads[30]<-sum(chickenheads[europe], na.rm=TRUE)
regionsources$heads[31]<-sum(duckheads[europe], na.rm=TRUE)
regionsources$heads[32]<-sum(horseheads[europe], na.rm=TRUE)
regionsources$heads[33]<-sum(camelheads[europe], na.rm=TRUE)
regionsources$heads[34]<-sum(muleheads[europe], na.rm=TRUE)
regionsources$heads[35]<-sum(assheads[europe], na.rm=TRUE)
regionsources$heads[36]<-sum(regionsources$heads[c(25:35)], na.rm=TRUE)

regionsources$heads[37]<-sum(cattleheads[latinamerica], na.rm=TRUE)
regionsources$heads[38]<-sum(buffaloheads[latinamerica], na.rm=TRUE)
regionsources$heads[39]<-sum(goatheads[latinamerica], na.rm=TRUE)
regionsources$heads[40]<-sum(sheepheads[latinamerica], na.rm=TRUE)
regionsources$heads[41]<-sum(pigheads[latinamerica], na.rm=TRUE)
regionsources$heads[42]<-sum(chickenheads[latinamerica], na.rm=TRUE)
regionsources$heads[43]<-sum(duckheads[latinamerica], na.rm=TRUE)
regionsources$heads[44]<-sum(horseheads[latinamerica], na.rm=TRUE)
regionsources$heads[45]<-sum(camelheads[latinamerica], na.rm=TRUE)
regionsources$heads[46]<-sum(muleheads[latinamerica], na.rm=TRUE)
regionsources$heads[47]<-sum(assheads[latinamerica], na.rm=TRUE)
regionsources$heads[48]<-sum(regionsources$heads[c(37:47)], na.rm=TRUE)

regionsources$heads[49]<-sum(cattleheads[mena], na.rm=TRUE)
regionsources$heads[50]<-sum(buffaloheads[mena], na.rm=TRUE)
regionsources$heads[51]<-sum(goatheads[mena], na.rm=TRUE)
regionsources$heads[52]<-sum(sheepheads[mena], na.rm=TRUE)
regionsources$heads[53]<-sum(pigheads[mena], na.rm=TRUE)
regionsources$heads[54]<-sum(chickenheads[mena], na.rm=TRUE)
regionsources$heads[55]<-sum(duckheads[mena], na.rm=TRUE)
regionsources$heads[56]<-sum(horseheads[mena], na.rm=TRUE)
regionsources$heads[57]<-sum(camelheads[mena], na.rm=TRUE)
regionsources$heads[58]<-sum(muleheads[mena], na.rm=TRUE)
regionsources$heads[59]<-sum(assheads[mena], na.rm=TRUE)
regionsources$heads[60]<-sum(regionsources$heads[c(49:59)], na.rm=TRUE)

regionsources$heads[61]<-sum(cattleheads[northamerica], na.rm=TRUE)
regionsources$heads[62]<-sum(buffaloheads[northamerica], na.rm=TRUE)
regionsources$heads[63]<-sum(goatheads[northamerica], na.rm=TRUE)
regionsources$heads[64]<-sum(sheepheads[northamerica], na.rm=TRUE)
regionsources$heads[65]<-sum(pigheads[northamerica], na.rm=TRUE)
regionsources$heads[66]<-sum(chickenheads[northamerica], na.rm=TRUE)
regionsources$heads[67]<-sum(duckheads[northamerica], na.rm=TRUE)
regionsources$heads[68]<-sum(horseheads[northamerica], na.rm=TRUE)
regionsources$heads[69]<-sum(camelheads[northamerica], na.rm=TRUE)
regionsources$heads[70]<-sum(muleheads[northamerica], na.rm=TRUE)
regionsources$heads[71]<-sum(assheads[northamerica], na.rm=TRUE)
regionsources$heads[72]<-sum(regionsources$heads[c(61:71)], na.rm=TRUE)

regionsources$heads[73]<-sum(cattleheads[oceania], na.rm=TRUE)
regionsources$heads[74]<-sum(buffaloheads[oceania], na.rm=TRUE)
regionsources$heads[75]<-sum(goatheads[oceania], na.rm=TRUE)
regionsources$heads[76]<-sum(sheepheads[oceania], na.rm=TRUE)
regionsources$heads[77]<-sum(pigheads[oceania], na.rm=TRUE)
regionsources$heads[78]<-sum(chickenheads[oceania], na.rm=TRUE)
regionsources$heads[79]<-sum(duckheads[oceania], na.rm=TRUE)
regionsources$heads[80]<-sum(horseheads[oceania], na.rm=TRUE)
regionsources$heads[81]<-sum(camelheads[oceania], na.rm=TRUE)
regionsources$heads[82]<-sum(muleheads[oceania], na.rm=TRUE)
regionsources$heads[83]<-sum(assheads[oceania], na.rm=TRUE)
regionsources$heads[84]<-sum(regionsources$heads[c(73:83)], na.rm=TRUE)

regionsources$emissionsperhead<-NA
regionsources$emissionsperhead<-regionsources$emissions/regionsources$heads

######### also split for the land, storage and storage survived ######

regionsources$emissions_L<-NA
regionsources$emissions_L[1]<-sum(cattleemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[2]<-sum(buffaloemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[3]<-sum(goatemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[4]<-sum(sheepemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[5]<-sum(pigemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[6]<-sum(chickenemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[7]<-sum(duckemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[8]<-sum(horseemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[9]<-sum(camelemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[10]<-sum(muleemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[11]<-sum(assemissions_L[africa], na.rm=TRUE)
regionsources$emissions_L[12]<-sum(regionsources$emissions_L[c(1:11)], na.rm=TRUE)

regionsources$emissions_L[13]<-sum(cattleemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[14]<-sum(buffaloemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[15]<-sum(goatemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[16]<-sum(sheepemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[17]<-sum(pigemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[18]<-sum(chickenemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[19]<-sum(duckemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[20]<-sum(horseemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[21]<-sum(camelemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[22]<-sum(muleemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[23]<-sum(assemissions_L[asia], na.rm=TRUE)
regionsources$emissions_L[24]<-sum(regionsources$emissions_L[c(13:23)], na.rm=TRUE)

regionsources$emissions_L[25]<-sum(cattleemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[26]<-sum(buffaloemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[27]<-sum(goatemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[28]<-sum(sheepemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[29]<-sum(pigemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[30]<-sum(chickenemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[31]<-sum(duckemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[32]<-sum(horseemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[33]<-sum(camelemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[34]<-sum(muleemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[35]<-sum(assemissions_L[europe], na.rm=TRUE)
regionsources$emissions_L[36]<-sum(regionsources$emissions_L[c(25:35)], na.rm=TRUE)

regionsources$emissions_L[37]<-sum(cattleemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[38]<-sum(buffaloemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[39]<-sum(goatemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[40]<-sum(sheepemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[41]<-sum(pigemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[42]<-sum(chickenemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[43]<-sum(duckemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[44]<-sum(horseemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[45]<-sum(camelemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[46]<-sum(muleemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[47]<-sum(assemissions_L[latinamerica], na.rm=TRUE)
regionsources$emissions_L[48]<-sum(regionsources$emissions_L[c(37:47)], na.rm=TRUE)

regionsources$emissions_L[49]<-sum(cattleemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[50]<-sum(buffaloemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[51]<-sum(goatemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[52]<-sum(sheepemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[53]<-sum(pigemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[54]<-sum(chickenemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[55]<-sum(duckemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[56]<-sum(horseemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[57]<-sum(camelemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[58]<-sum(muleemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[59]<-sum(assemissions_L[mena], na.rm=TRUE)
regionsources$emissions_L[60]<-sum(regionsources$emissions_L[c(49:59)], na.rm=TRUE)

regionsources$emissions_L[61]<-sum(cattleemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[62]<-sum(buffaloemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[63]<-sum(goatemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[64]<-sum(sheepemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[65]<-sum(pigemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[66]<-sum(chickenemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[67]<-sum(duckemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[68]<-sum(horseemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[69]<-sum(camelemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[70]<-sum(muleemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[71]<-sum(assemissions_L[northamerica], na.rm=TRUE)
regionsources$emissions_L[72]<-sum(regionsources$emissions_L[c(61:71)], na.rm=TRUE)

regionsources$emissions_L[73]<-sum(cattleemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[74]<-sum(buffaloemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[75]<-sum(goatemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[76]<-sum(sheepemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[77]<-sum(pigemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[78]<-sum(chickenemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[79]<-sum(duckemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[80]<-sum(horseemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[81]<-sum(camelemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[82]<-sum(muleemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[83]<-sum(assemissions_L[oceania], na.rm=TRUE)
regionsources$emissions_L[84]<-sum(regionsources$emissions_L[c(73:83)], na.rm=TRUE)

regionsources$emissions_S<-NA
regionsources$emissions_S[1]<-sum(cattleemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[2]<-sum(buffaloemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[3]<-sum(goatemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[4]<-sum(sheepemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[5]<-sum(pigemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[6]<-sum(chickenemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[7]<-sum(duckemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[8]<-sum(horseemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[9]<-sum(camelemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[10]<-sum(muleemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[11]<-sum(assemissions_S[africa], na.rm=TRUE)
regionsources$emissions_S[12]<-sum(regionsources$emissions_S[c(1:11)], na.rm=TRUE)

regionsources$emissions_S[13]<-sum(cattleemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[14]<-sum(buffaloemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[15]<-sum(goatemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[16]<-sum(sheepemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[17]<-sum(pigemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[18]<-sum(chickenemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[19]<-sum(duckemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[20]<-sum(horseemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[21]<-sum(camelemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[22]<-sum(muleemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[23]<-sum(assemissions_S[asia], na.rm=TRUE)
regionsources$emissions_S[24]<-sum(regionsources$emissions_S[c(13:23)], na.rm=TRUE)

regionsources$emissions_S[25]<-sum(cattleemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[26]<-sum(buffaloemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[27]<-sum(goatemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[28]<-sum(sheepemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[29]<-sum(pigemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[30]<-sum(chickenemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[31]<-sum(duckemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[32]<-sum(horseemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[33]<-sum(camelemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[34]<-sum(muleemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[35]<-sum(assemissions_S[europe], na.rm=TRUE)
regionsources$emissions_S[36]<-sum(regionsources$emissions_S[c(25:35)], na.rm=TRUE)

regionsources$emissions_S[37]<-sum(cattleemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[38]<-sum(buffaloemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[39]<-sum(goatemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[40]<-sum(sheepemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[41]<-sum(pigemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[42]<-sum(chickenemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[43]<-sum(duckemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[44]<-sum(horseemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[45]<-sum(camelemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[46]<-sum(muleemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[47]<-sum(assemissions_S[latinamerica], na.rm=TRUE)
regionsources$emissions_S[48]<-sum(regionsources$emissions_S[c(37:47)], na.rm=TRUE)

regionsources$emissions_S[49]<-sum(cattleemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[50]<-sum(buffaloemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[51]<-sum(goatemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[52]<-sum(sheepemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[53]<-sum(pigemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[54]<-sum(chickenemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[55]<-sum(duckemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[56]<-sum(horseemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[57]<-sum(camelemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[58]<-sum(muleemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[59]<-sum(assemissions_S[mena], na.rm=TRUE)
regionsources$emissions_S[60]<-sum(regionsources$emissions_S[c(49:59)], na.rm=TRUE)

regionsources$emissions_S[61]<-sum(cattleemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[62]<-sum(buffaloemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[63]<-sum(goatemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[64]<-sum(sheepemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[65]<-sum(pigemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[66]<-sum(chickenemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[67]<-sum(duckemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[68]<-sum(horseemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[69]<-sum(camelemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[70]<-sum(muleemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[71]<-sum(assemissions_S[northamerica], na.rm=TRUE)
regionsources$emissions_S[72]<-sum(regionsources$emissions_S[c(61:71)], na.rm=TRUE)

regionsources$emissions_S[73]<-sum(cattleemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[74]<-sum(buffaloemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[75]<-sum(goatemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[76]<-sum(sheepemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[77]<-sum(pigemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[78]<-sum(chickenemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[79]<-sum(duckemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[80]<-sum(horseemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[81]<-sum(camelemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[82]<-sum(muleemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[83]<-sum(assemissions_S[oceania], na.rm=TRUE)
regionsources$emissions_S[84]<-sum(regionsources$emissions_S[c(73:83)], na.rm=TRUE)


regionsources$emissions_S_survived<-NA
regionsources$emissions_S_survived[1]<-sum(cattleemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[2]<-sum(buffaloemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[3]<-sum(goatemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[4]<-sum(sheepemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[5]<-sum(pigemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[6]<-sum(chickenemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[7]<-sum(duckemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[8]<-sum(horseemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[9]<-sum(camelemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[10]<-sum(muleemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[11]<-sum(assemissions_S_survived[africa], na.rm=TRUE)
regionsources$emissions_S_survived[12]<-sum(regionsources$emissions_S_survived[c(1:12)], na.rm=TRUE)

regionsources$emissions_S_survived[13]<-sum(cattleemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[14]<-sum(buffaloemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[15]<-sum(goatemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[16]<-sum(sheepemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[17]<-sum(pigemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[18]<-sum(chickenemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[19]<-sum(duckemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[20]<-sum(horseemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[21]<-sum(camelemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[22]<-sum(muleemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[23]<-sum(assemissions_S_survived[asia], na.rm=TRUE)
regionsources$emissions_S_survived[24]<-sum(regionsources$emissions_S_survived[c(13:23)], na.rm=TRUE)

regionsources$emissions_S_survived[25]<-sum(cattleemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[26]<-sum(buffaloemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[27]<-sum(goatemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[28]<-sum(sheepemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[29]<-sum(pigemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[30]<-sum(chickenemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[31]<-sum(duckemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[32]<-sum(horseemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[33]<-sum(camelemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[34]<-sum(muleemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[35]<-sum(assemissions_S_survived[europe], na.rm=TRUE)
regionsources$emissions_S_survived[36]<-sum(regionsources$emissions_S_survived[c(25:35)], na.rm=TRUE)

regionsources$emissions_S_survived[37]<-sum(cattleemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[38]<-sum(buffaloemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[39]<-sum(goatemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[40]<-sum(sheepemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[41]<-sum(pigemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[42]<-sum(chickenemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[43]<-sum(duckemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[44]<-sum(horseemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[45]<-sum(camelemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[46]<-sum(muleemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[47]<-sum(assemissions_S_survived[latinamerica], na.rm=TRUE)
regionsources$emissions_S_survived[48]<-sum(regionsources$emissions_S_survived[c(37:47)], na.rm=TRUE)

regionsources$emissions_S_survived[49]<-sum(cattleemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[50]<-sum(buffaloemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[51]<-sum(goatemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[52]<-sum(sheepemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[53]<-sum(pigemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[54]<-sum(chickenemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[55]<-sum(duckemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[56]<-sum(horseemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[57]<-sum(camelemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[58]<-sum(muleemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[59]<-sum(assemissions_S_survived[mena], na.rm=TRUE)
regionsources$emissions_S_survived[60]<-sum(regionsources$emissions_S_survived[c(49:59)], na.rm=TRUE)

regionsources$emissions_S_survived[61]<-sum(cattleemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[62]<-sum(buffaloemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[63]<-sum(goatemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[64]<-sum(sheepemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[65]<-sum(pigemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[66]<-sum(chickenemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[67]<-sum(duckemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[68]<-sum(horseemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[69]<-sum(camelemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[70]<-sum(muleemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[71]<-sum(assemissions_S_survived[northamerica], na.rm=TRUE)
regionsources$emissions_S_survived[72]<-sum(regionsources$emissions_S_survived[c(61:71)], na.rm=TRUE)

regionsources$emissions_S_survived[73]<-sum(cattleemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[74]<-sum(buffaloemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[75]<-sum(goatemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[76]<-sum(sheepemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[77]<-sum(pigemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[78]<-sum(chickenemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[79]<-sum(duckemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[80]<-sum(horseemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[81]<-sum(camelemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[82]<-sum(muleemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[83]<-sum(assemissions_S_survived[oceania], na.rm=TRUE)
regionsources$emissions_S_survived[84]<-sum(regionsources$emissions_S_survived[c(73:83)], na.rm=TRUE)

##### and also split for intensive extensive ######

regionsources$emissions_I<-NA
regionsources$emissions_I[1]<-sum(cattleemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[2]<-sum(buffaloemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[3]<-sum(goatemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[4]<-sum(sheepemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[5]<-sum(pigemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[6]<-sum(chickenemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[7]<-sum(duckemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[8]<-sum(horseemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[9]<-sum(camelemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[10]<-sum(muleemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[11]<-sum(assemissions_I[africa], na.rm=TRUE)
regionsources$emissions_I[12]<-sum(regionsources$emissions_I[c(1:12)], na.rm=TRUE)

regionsources$emissions_I[13]<-sum(cattleemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[14]<-sum(buffaloemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[15]<-sum(goatemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[16]<-sum(sheepemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[17]<-sum(pigemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[18]<-sum(chickenemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[19]<-sum(duckemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[20]<-sum(horseemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[21]<-sum(camelemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[22]<-sum(muleemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[23]<-sum(assemissions_I[asia], na.rm=TRUE)
regionsources$emissions_I[24]<-sum(regionsources$emissions_I[c(13:23)], na.rm=TRUE)


regionsources$emissions_I[25]<-sum(cattleemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[26]<-sum(buffaloemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[27]<-sum(goatemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[28]<-sum(sheepemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[29]<-sum(pigemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[30]<-sum(chickenemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[31]<-sum(duckemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[32]<-sum(horseemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[33]<-sum(camelemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[34]<-sum(muleemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[35]<-sum(assemissions_I[europe], na.rm=TRUE)
regionsources$emissions_I[36]<-sum(regionsources$emissions_I[c(25:35)], na.rm=TRUE)


regionsources$emissions_I[37]<-sum(cattleemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[38]<-sum(buffaloemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[39]<-sum(goatemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[40]<-sum(sheepemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[41]<-sum(pigemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[42]<-sum(chickenemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[43]<-sum(duckemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[44]<-sum(horseemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[45]<-sum(camelemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[46]<-sum(muleemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[47]<-sum(assemissions_I[latinamerica], na.rm=TRUE)
regionsources$emissions_I[48]<-sum(regionsources$emissions_I[c(37:47)], na.rm=TRUE)


regionsources$emissions_I[49]<-sum(cattleemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[50]<-sum(buffaloemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[51]<-sum(goatemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[52]<-sum(sheepemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[53]<-sum(pigemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[54]<-sum(chickenemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[55]<-sum(duckemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[56]<-sum(horseemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[57]<-sum(camelemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[58]<-sum(muleemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[59]<-sum(assemissions_I[mena], na.rm=TRUE)
regionsources$emissions_I[60]<-sum(regionsources$emissions_I[c(49:59)], na.rm=TRUE)


regionsources$emissions_I[61]<-sum(cattleemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[62]<-sum(buffaloemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[63]<-sum(goatemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[64]<-sum(sheepemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[65]<-sum(pigemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[66]<-sum(chickenemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[67]<-sum(duckemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[68]<-sum(horseemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[69]<-sum(camelemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[70]<-sum(muleemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[71]<-sum(assemissions_I[northamerica], na.rm=TRUE)
regionsources$emissions_I[72]<-sum(regionsources$emissions_I[c(61:71)], na.rm=TRUE)


regionsources$emissions_I[73]<-sum(cattleemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[74]<-sum(buffaloemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[75]<-sum(goatemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[76]<-sum(sheepemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[77]<-sum(pigemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[78]<-sum(chickenemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[79]<-sum(duckemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[80]<-sum(horseemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[81]<-sum(camelemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[82]<-sum(muleemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[83]<-sum(assemissions_I[oceania], na.rm=TRUE)
regionsources$emissions_I[84]<-sum(regionsources$emissions_I[c(73:83)], na.rm=TRUE)



filename<-paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput/diffuse_regionalatrribution",i, ".csv")
write.csv(regionsources,file=filename) #, overwrite=TRUE




