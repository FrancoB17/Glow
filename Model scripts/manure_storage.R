#This script calculates the fraction of manure going to different storage systems
# and the survival of Cryptosporidium in these systems
# This script is based on the work by Jorien and adapted by Lucie
# error by Jorien not splitting up 'used for fuel' has been fixed
# Dry lot is now included as storage system, unlike Joriens assumption
# temperature-dependent survival is now included
# September 2016
# this script is called by calling_diffuse_emissions.R

#############  storage systems #############
MMS <- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/ManureManagementSystems.csv")
MMSn <- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/ManureManagementSystemsnew.csv")

#Overzicht codes in deze file
#PP = pasture/range/paddock
#DS = daily spread
#SS = solid storage
#DL = dry lot
#LS = liquid/slurry
#UAL = uncovered anaerobic lagoon
#AD = anaerobic digester
#BF = burned for fuel
#O = other systems
#Pl1 = pit storage lower than 1 month
#Ph1 = pit storage higher than 1 month
#Tot = sum of all systems fractions
#Tot2 = sum of all storage systems fractions (so excluding pasture etc.)

### Preparing Calculations
#For USEPA Safley data, the category used for fuel is said to contain both anaerobic digestion and burned for fuel. 
# We consider the first a storage system, and the second not. 
# therefore we create an extra category (AD) and assume both are half of the reported value. 
# do not repeat this or it will go wrong

MMS$AD_poultry<-MMS$BF_poultry*0.5
MMS$BF_poultry<-MMS$BF_poultry*0.5

MMS$AD_sheep<-MMS$BF_sheep*0.5
MMS$BF_sheep<-MMS$BF_sheep*0.5

MMS$AD_goats<-MMS$BF_goats*0.5
MMS$BF_goats<-MMS$BF_goats*0.5

MMS$AD_horses<-MMS$BF_horses*0.5
MMS$BF_horses<-MMS$BF_horses*0.5

MMS$AD_asses<-MMS$BF_asses*0.5
MMS$BF_asses<-MMS$BF_asses*0.5

MMS$AD_mules<-MMS$BF_mules*0.5
MMS$BF_mules<-MMS$BF_mules*0.5

MMS$AD_camels<-MMS$BF_camels*0.5
MMS$BF_camels<-MMS$BF_camels*0.5

#Use ratios to recalculate percentages with only manure management systems categorised as storage system
# IPCC data
MMSn$DS_cattle <- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$DS_cattle
MMSn$SS_cattle <- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$SS_cattle
MMSn$LS_cattle <- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$LS_cattle
MMSn$UAL_cattle<- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$UAL_cattle
MMSn$AD_cattle <- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$AD_cattle
MMSn$O_cattle <- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$O_cattle
MMSn$DL_cattle <- (1/(MMS$DS_cattle+MMS$SS_cattle+MMS$LS_cattle+MMS$UAL_cattle+MMS$AD_cattle+MMS$O_cattle+MMS$DL_cattle))*MMS$DL_cattle
MMSn$tot_cattle <- MMSn$DS_cattle + MMSn$SS_cattle + MMSn$LS_cattle + MMSn$UAL_cattle + MMSn$AD_cattle + MMSn$O_cattle + MMSn$DL_cattle

MMSn$DS_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$DS_buffaloes
MMSn$SS_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$SS_buffaloes
MMSn$LS_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$LS_buffaloes
MMSn$UAL_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$UAL_buffaloes
MMSn$AD_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$AD_buffaloes
MMSn$O_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$O_buffaloes
MMSn$DL_buffaloes <- (1/(MMS$DS_buffaloes+MMS$SS_buffaloes+MMS$LS_buffaloes+MMS$UAL_buffaloes+MMS$AD_buffaloes+MMS$O_buffaloes+MMS$DL_buffaloes))*MMS$DL_buffaloes
MMSn$tot_buffaloes <- MMSn$DS_buffaloes + MMSn$SS_buffaloes + MMSn$LS_buffaloes + MMSn$UAL_buffaloes + MMSn$AD_buffaloes + MMSn$O_buffaloes + MMSn$DL_buffaloes

MMSn$DS_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$DS_pigs
MMSn$SS_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$SS_pigs
MMSn$LS_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$LS_pigs
MMSn$UAL_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$UAL_pigs
MMSn$Pl1_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$Pl1_pigs
MMSn$Ph1_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$Ph1_pigs
MMSn$AD_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$AD_pigs
MMSn$O_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$O_pigs
MMSn$DL_pigs <- (1/(MMS$DS_pigs+MMS$SS_pigs+MMS$LS_pigs+MMS$UAL_pigs+MMS$O_pigs+MMS$Pl1_pigs+MMS$Ph1_pigs+MMS$AD_pigs+MMS$DL_pigs))*MMS$DL_pigs
MMSn$tot_pigs <- MMSn$DS_pigs + MMSn$SS_pigs + MMSn$LS_pigs + MMSn$UAL_pigs + MMSn$AD_pigs + MMSn$O_pigs + MMSn$DL_pigs + MMSn$Pl1_pigs + MMSn$Ph1_pigs


#USEPA Safley data
MMSn$UAL_poultry <- (1/(MMS$UAL_poultry+MMS$LS_poultry+MMS$DS_poultry+MMS$SSDL_poultry+MMS$O_poultry+MMS$AD_poultry))*MMS$UAL_poultry
MMSn$LS_poultry <- (1/(MMS$UAL_poultry+MMS$LS_poultry+MMS$DS_poultry+MMS$SSDL_poultry+MMS$O_poultry+MMS$AD_poultry))*MMS$LS_poultry
MMSn$DS_poultry <- (1/(MMS$UAL_poultry+MMS$LS_poultry+MMS$DS_poultry+MMS$SSDL_poultry+MMS$O_poultry+MMS$AD_poultry))*MMS$DS_poultry
MMSn$SSDL_poultry <- (1/(MMS$UAL_poultry+MMS$LS_poultry+MMS$DS_poultry+MMS$SSDL_poultry+MMS$O_poultry+MMS$AD_poultry))*MMS$SSDL_poultry
MMSn$O_poultry <- (1/(MMS$UAL_poultry+MMS$LS_poultry+MMS$DS_poultry+MMS$SSDL_poultry+MMS$O_poultry+MMS$AD_poultry))*MMS$O_poultry
MMSn$AD_poultry <- (1/(MMS$UAL_poultry+MMS$LS_poultry+MMS$DS_poultry+MMS$SSDL_poultry+MMS$O_poultry+MMS$AD_poultry))*MMS$AD_poultry
MMSn$tot_poultry <- MMSn$DS_poultry + MMSn$SSDL_poultry + MMSn$LS_poultry + MMSn$UAL_poultry + MMSn$AD_poultry + MMSn$O_poultry 

MMSn$UAL_sheep <- (1/(MMS$UAL_sheep+MMS$LS_sheep+MMS$DS_sheep+MMS$SSDL_sheep+MMS$O_sheep+MMS$AD_sheep))*MMS$UAL_sheep
MMSn$LS_sheep <- (1/(MMS$UAL_sheep+MMS$LS_sheep+MMS$DS_sheep+MMS$SSDL_sheep+MMS$O_sheep+MMS$AD_sheep))*MMS$LS_sheep
MMSn$DS_sheep <- (1/(MMS$UAL_sheep+MMS$LS_sheep+MMS$DS_sheep+MMS$SSDL_sheep+MMS$O_sheep+MMS$AD_sheep))*MMS$DS_sheep
MMSn$SSDL_sheep <- (1/(MMS$UAL_sheep+MMS$LS_sheep+MMS$DS_sheep+MMS$SSDL_sheep+MMS$O_sheep+MMS$AD_sheep))*MMS$SSDL_sheep
MMSn$O_sheep <- (1/(MMS$UAL_sheep+MMS$LS_sheep+MMS$DS_sheep+MMS$SSDL_sheep+MMS$O_sheep+MMS$AD_sheep))*MMS$O_sheep
MMSn$AD_sheep <- (1/(MMS$UAL_sheep+MMS$LS_sheep+MMS$DS_sheep+MMS$SSDL_sheep+MMS$O_sheep+MMS$AD_sheep))*MMS$AD_sheep
MMSn$tot_sheep <- MMSn$DS_sheep + MMSn$SSDL_sheep + MMSn$LS_sheep + MMSn$UAL_sheep + MMSn$AD_sheep + MMSn$O_sheep 

MMSn$UAL_goats <- (1/(MMS$UAL_goats+MMS$LS_goats+MMS$DS_goats+MMS$SSDL_goats+MMS$O_goats+MMS$AD_goats))*MMS$UAL_goats
MMSn$LS_goats <- (1/(MMS$UAL_goats+MMS$LS_goats+MMS$DS_goats+MMS$SSDL_goats+MMS$O_goats+MMS$AD_goats))*MMS$LS_goats
MMSn$DS_goats <- (1/(MMS$UAL_goats+MMS$LS_goats+MMS$DS_goats+MMS$SSDL_goats+MMS$O_goats+MMS$AD_goats))*MMS$DS_goats
MMSn$SSDL_goats <- (1/(MMS$UAL_goats+MMS$LS_goats+MMS$DS_goats+MMS$SSDL_goats+MMS$O_goats+MMS$AD_goats))*MMS$SSDL_goats
MMSn$O_goats <- (1/(MMS$UAL_goats+MMS$LS_goats+MMS$DS_goats+MMS$SSDL_goats+MMS$O_goats+MMS$AD_goats))*MMS$O_goats
MMSn$AD_goats <- (1/(MMS$UAL_goats+MMS$LS_goats+MMS$DS_goats+MMS$SSDL_goats+MMS$O_goats+MMS$AD_goats))*MMS$AD_goats
MMSn$tot_goats <- MMSn$DS_goats + MMSn$SSDL_goats + MMSn$LS_goats + MMSn$UAL_goats + MMSn$AD_goats + MMSn$O_goats 

MMSn$UAL_horses <- (1/(MMS$UAL_horses+MMS$LS_horses+MMS$DS_horses+MMS$SSDL_horses+MMS$O_horses+MMS$AD_horses))*MMS$UAL_horses
MMSn$LS_horses <- (1/(MMS$UAL_horses+MMS$LS_horses+MMS$DS_horses+MMS$SSDL_horses+MMS$O_horses+MMS$AD_horses))*MMS$LS_horses
MMSn$DS_horses <- (1/(MMS$UAL_horses+MMS$LS_horses+MMS$DS_horses+MMS$SSDL_horses+MMS$O_horses+MMS$AD_horses))*MMS$DS_horses
MMSn$SSDL_horses <- (1/(MMS$UAL_horses+MMS$LS_horses+MMS$DS_horses+MMS$SSDL_horses+MMS$O_horses+MMS$AD_horses))*MMS$SSDL_horses
MMSn$O_horses <- (1/(MMS$UAL_horses+MMS$LS_horses+MMS$DS_horses+MMS$SSDL_horses+MMS$O_horses+MMS$AD_horses))*MMS$O_horses
MMSn$AD_horses <- (1/(MMS$UAL_horses+MMS$LS_horses+MMS$DS_horses+MMS$SSDL_horses+MMS$O_horses+MMS$AD_horses))*MMS$AD_horses
MMSn$tot_horses <- MMSn$DS_horses + MMSn$SSDL_horses + MMSn$LS_horses + MMSn$UAL_horses + MMSn$AD_horses + MMSn$O_horses 

MMSn$UAL_asses <- (1/(MMS$UAL_asses+MMS$LS_asses+MMS$DS_asses+MMS$SSDL_asses+MMS$O_asses+MMS$AD_asses))*MMS$UAL_asses
MMSn$LS_asses <- (1/(MMS$UAL_asses+MMS$LS_asses+MMS$DS_asses+MMS$SSDL_asses+MMS$O_asses+MMS$AD_asses))*MMS$LS_asses
MMSn$DS_asses <- (1/(MMS$UAL_asses+MMS$LS_asses+MMS$DS_asses+MMS$SSDL_asses+MMS$O_asses+MMS$AD_asses))*MMS$DS_asses
MMSn$SSDL_asses <- (1/(MMS$UAL_asses+MMS$LS_asses+MMS$DS_asses+MMS$SSDL_asses+MMS$O_asses+MMS$AD_asses))*MMS$SSDL_asses
MMSn$O_asses <- (1/(MMS$UAL_asses+MMS$LS_asses+MMS$DS_asses+MMS$SSDL_asses+MMS$O_asses+MMS$AD_asses))*MMS$O_asses
MMSn$AD_asses <- (1/(MMS$UAL_asses+MMS$LS_asses+MMS$DS_asses+MMS$SSDL_asses+MMS$O_asses+MMS$AD_asses))*MMS$AD_asses
MMSn$tot_asses <- MMSn$DS_asses + MMSn$SSDL_asses + MMSn$LS_asses + MMSn$UAL_asses + MMSn$AD_asses + MMSn$O_asses 

MMSn$UAL_mules <- (1/(MMS$UAL_mules+MMS$LS_mules+MMS$DS_mules+MMS$SSDL_mules+MMS$O_mules+MMS$AD_mules))*MMS$UAL_mules
MMSn$LS_mules <- (1/(MMS$UAL_mules+MMS$LS_mules+MMS$DS_mules+MMS$SSDL_mules+MMS$O_mules+MMS$AD_mules))*MMS$LS_mules
MMSn$DS_mules <- (1/(MMS$UAL_mules+MMS$LS_mules+MMS$DS_mules+MMS$SSDL_mules+MMS$O_mules+MMS$AD_mules))*MMS$DS_mules
MMSn$SSDL_mules <- (1/(MMS$UAL_mules+MMS$LS_mules+MMS$DS_mules+MMS$SSDL_mules+MMS$O_mules+MMS$AD_mules))*MMS$SSDL_mules
MMSn$O_mules <- (1/(MMS$UAL_mules+MMS$LS_mules+MMS$DS_mules+MMS$SSDL_mules+MMS$O_mules+MMS$AD_mules))*MMS$O_mules
MMSn$AD_mules <- (1/(MMS$UAL_mules+MMS$LS_mules+MMS$DS_mules+MMS$SSDL_mules+MMS$O_mules+MMS$AD_mules))*MMS$AD_mules
MMSn$tot_mules <- MMSn$DS_mules + MMSn$SSDL_mules + MMSn$LS_mules + MMSn$UAL_mules + MMSn$AD_mules + MMSn$O_mules 

MMSn$UAL_camels <- (1/(MMS$UAL_camels+MMS$LS_camels+MMS$DS_camels+MMS$SSDL_camels+MMS$O_camels+MMS$AD_camels))*MMS$UAL_camels
MMSn$LS_camels <- (1/(MMS$UAL_camels+MMS$LS_camels+MMS$DS_camels+MMS$SSDL_camels+MMS$O_camels+MMS$AD_camels))*MMS$LS_camels
MMSn$DS_camels <- (1/(MMS$UAL_camels+MMS$LS_camels+MMS$DS_camels+MMS$SSDL_camels+MMS$O_camels+MMS$AD_camels))*MMS$DS_camels
MMSn$SSDL_camels <- (1/(MMS$UAL_camels+MMS$LS_camels+MMS$DS_camels+MMS$SSDL_camels+MMS$O_camels+MMS$AD_camels))*MMS$SSDL_camels
MMSn$O_camels <- (1/(MMS$UAL_camels+MMS$LS_camels+MMS$DS_camels+MMS$SSDL_camels+MMS$O_camels+MMS$AD_camels))*MMS$O_camels
MMSn$AD_camels <- (1/(MMS$UAL_camels+MMS$LS_camels+MMS$DS_camels+MMS$SSDL_camels+MMS$O_camels+MMS$AD_camels))*MMS$AD_camels
MMSn$tot_camels <- MMSn$DS_camels + MMSn$SSDL_camels + MMSn$LS_camels + MMSn$UAL_camels + MMSn$AD_camels + MMSn$O_camels 

# change all NA and NaN to zero
MMSn[is.na(MMSn)] <- 0

#write away file, for in-between logging
#write.csv(MMSn,file="D:/LuciePhD/Model/Cryptomodel/modeldata/manure/ManureManagementSystemscalculated.csv")
#read in file again, to start from here
#MMSn <- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/ManureManagementSystemscalculated.csv")

# calculating fraction surviving in systems in which survival is not temperature dependent (daily spread and anaerobic digester)
MMSn$survival_DSAD_cattle <- MMSn$DS_cattle + (0.5*0.01*MMSn$AD_cattle)
# calculate fraction going to all other systems
MMSn$survival_other_cattle <- 1-(MMSn$DS_cattle + MMSn$AD_cattle)

MMSn$survival_DSAD_buffaloes <- MMSn$DS_buffaloes + (0.5*0.01*MMSn$AD_buffaloes)
MMSn$survival_other_buffaloes <- 1-(MMSn$DS_buffaloes + MMSn$AD_buffaloes)

MMSn$survival_DSAD_pigs <- MMSn$DS_pigs + (0.5*0.01*MMSn$AD_pigs)
MMSn$survival_other_pigs <- 1-(MMSn$DS_pigs + MMSn$AD_pigs + MMSn$Pl1_pigs)
#only for pigs: there is one storage system for which a shorter storage time is assumed (pit low)
MMSn$survival_pitlow_pigs <- MMSn$Pl1_pigs

MMSn$survival_DSAD_poultry <- MMSn$DS_poultry + (0.5*0.01*MMSn$AD_poultry)
MMSn$survival_other_poultry <- 1-(MMSn$DS_poultry + MMSn$AD_poultry)

MMSn$survival_DSAD_sheep <- MMSn$DS_sheep + (0.5*0.01*MMSn$AD_sheep)
MMSn$survival_other_sheep <- 1-(MMSn$DS_sheep + MMSn$AD_sheep)

MMSn$survival_DSAD_goats <- MMSn$DS_goats + (0.5*0.01*MMSn$AD_goats)
MMSn$survival_other_goats <- 1-(MMSn$DS_goats + MMSn$AD_goats)

MMSn$survival_DSAD_horses <- MMSn$DS_horses + (0.5*0.01*MMSn$AD_horses)
MMSn$survival_other_horses <- 1-(MMSn$DS_horses + MMSn$AD_horses)

MMSn$survival_DSAD_asses <- MMSn$DS_asses + (0.5*0.01*MMSn$AD_asses)
MMSn$survival_other_asses <- 1-(MMSn$DS_asses + MMSn$AD_asses)

MMSn$survival_DSAD_mules <- MMSn$DS_mules + (0.5*0.01*MMSn$AD_mules)
MMSn$survival_other_mules <- 1-(MMSn$DS_mules + MMSn$AD_mules)

MMSn$survival_DSAD_camels <- MMSn$DS_camels + (0.5*0.01*MMSn$AD_camels)
MMSn$survival_other_camels <- 1-(MMSn$DS_camels + MMSn$AD_camels)

MMSn$survival_DSAD_cattle <- MMSn$DS_cattle + (0.5*0.01*MMSn$AD_cattle)
MMSn$survival_other_cattle <- 1-(MMSn$DS_cattle + MMSn$AD_cattle)


#Make a grid of alive fractions for categories DS and AD (daily spread and anaerobic digester)
cattlef1 <- data.frame(iso=MMSn$iso, cattlef1=MMSn$survival_DSAD_cattle)
Fcattle_DSAD <- subs(countries, cattlef1, subsWithNA=T)
buffaloesf1 <- data.frame(iso=MMSn$iso, buffaloesf1=MMSn$survival_DSAD_buffaloes)
Fbuffaloes_DSAD <- subs(countries, buffaloesf1, subsWithNA=T)
pigsf1 <- data.frame(iso=MMSn$iso, pigsf1=MMSn$survival_DSAD_pigs)
Fpigs_DSAD <- subs(countries, pigsf1, subsWithNA=T)
poultryf1 <- data.frame(iso=MMSn$iso, poultryf1=MMSn$survival_DSAD_poultry)
Fpoultry_DSAD <- subs(countries, poultryf1, subsWithNA=T)
sheepf1 <- data.frame(iso=MMSn$iso, sheepf1=MMSn$survival_DSAD_sheep)
Fsheep_DSAD <- subs(countries, sheepf1, subsWithNA=T)
goatsf1 <- data.frame(iso=MMSn$iso, goatsf1=MMSn$survival_DSAD_goats)
Fgoats_DSAD <- subs(countries, goatsf1, subsWithNA=T)
horsesf1 <- data.frame(iso=MMSn$iso, horsesf1=MMSn$survival_DSAD_horses)
Fhorses_DSAD <- subs(countries, horsesf1, subsWithNA=T)
assesf1 <- data.frame(iso=MMSn$iso, assesf1=MMSn$survival_DSAD_asses)
Fasses_DSAD <- subs(countries, assesf1, subsWithNA=T)
mulesf1 <- data.frame(iso=MMSn$iso, mulesf1=MMSn$survival_DSAD_mules)
Fmules_DSAD <- subs(countries, mulesf1, subsWithNA=T)
camelsf1 <- data.frame(iso=MMSn$iso, camelsf1=MMSn$survival_DSAD_camels)
Fcamels_DSAD <- subs(countries, camelsf1, subsWithNA=T)

#plot(Fcattle_DSAD, main= "cattle DSAD")
#plot(Fbuffaloes_DSAD, main = "buffaloes DSAD")
#plot(Fpigs_DSAD, main="pigs DSAD")
#plot(Fpoultry_DSAD, main ="poultry DSAD")
#plot(Fsheep_DSAD, main ="sheep DSAD")
#plot(Fgoats_DSAD, main= "goats DSAD")
#plot(Fhorses_DSAD, main= "horses DSAD")
#plot(Fasses_DSAD, main= "asses DSAD")
#plot(Fmules_DSAD, main= "mules DSAD")
#plot(Fcamels_DSAD, main= "camels DSAD")
# it can be seen that for goats, horses, asses, mules and camels this is exactly the same


# do the same for temperature-dependent survival, make grids
cattlef2 <- data.frame(iso=MMSn$iso, cattlef2=MMSn$survival_other_cattle)
Fcattle_other <- subs(countries, cattlef2, subsWithNA=T)
buffaloesf2 <- data.frame(iso=MMSn$iso, buffaloesf2=MMSn$survival_other_buffaloes)
Fbuffaloes_other <- subs(countries, buffaloesf2, subsWithNA=T)
pigsf2 <- data.frame(iso=MMSn$iso, pigsf2=MMSn$survival_other_pigs)
Fpigs_other <- subs(countries, pigsf2, subsWithNA=T)
poultryf2 <- data.frame(iso=MMSn$iso, poultryf2=MMSn$survival_other_poultry)
Fpoultry_other <- subs(countries, poultryf2, subsWithNA=T)
sheepf2 <- data.frame(iso=MMSn$iso, sheepf2=MMSn$survival_other_sheep)
Fsheep_other <- subs(countries, sheepf2, subsWithNA=T)
goatsf2 <- data.frame(iso=MMSn$iso, goatsf2=MMSn$survival_other_goats)
Fgoats_other <- subs(countries, goatsf2, subsWithNA=T)
horsesf2 <- data.frame(iso=MMSn$iso, horsesf2=MMSn$survival_other_horses)
Fhorses_other <- subs(countries, horsesf2, subsWithNA=T)
assesf2 <- data.frame(iso=MMSn$iso, assesf2=MMSn$survival_other_asses)
Fasses_other <- subs(countries, assesf2, subsWithNA=T)
mulesf2 <- data.frame(iso=MMSn$iso, mulesf2=MMSn$survival_other_mules)
Fmules_other <- subs(countries, mulesf2, subsWithNA=T)
camelsf2 <- data.frame(iso=MMSn$iso, camelsf2=MMSn$survival_other_camels)
Fcamels_other <- subs(countries, camelsf2, subsWithNA=T)

#plot(Fcattle_other, main= "cattle other")
#plot(Fbuffaloes_other, main = "buffaloes other")
#plot(Fpigs_other, main="pigs other")
#plot(Fpoultry_other, main ="poultry other")
#plot(Fsheep_other, main ="sheep other")
#plot(Fgoats_other, main= "goats other")
#plot(Fhorses_other, main= "horses other")
#plot(Fasses_other, main= "asses other")
#plot(Fmules_other, main= "mules other")
#plot(Fcamels_other, main= "camels other")

#make a grid for temperature-dependent survival for short storage in pits (pigs only)
pigsf3 <- data.frame(iso=MMSn$iso, pigsf3=MMSn$survival_pitlow_pigs)
Fpigs_pitlow <- subs(countries, pigsf3, subsWithNA=T)
#plot(Fpigs_pitlow, main="Pigs pit low")

Tair<- raster(paste0("D:/LuciePhD/Model/Cryptomodel/modeldata/Temperature/Tair/",par$temperature[i]))

# How T90 depends on temperature y = -2.5586x + 119.63
# How the K value can be calculated from T90 =-(LN(0.1)/T90)
# How survival depends on K value and time survival=e^(-k*t)


#Calculate T90 value based on relation between temperature and T90 from literature
#based on studies looking at oocyst viability or infectivity
temperature1<-par$temp_par1[i]
temperature2<-par$temp_par2[i]

#T90 <- temperature1*Tair + temperature2
#plot(T90, main="T90")

# calculate K value from T90
#K <- -1*(log(0.1)/T90)
#plot(K, main = "K")


########## calculations on vectors, instead of raster! great idea!
#Tair has values specific to 5 digits, not useful
#values<-unique(Tair)

#therefore, we round to 1 digit, to get a more manageable number of entries
#we add these to a dataframe
Tair_rounded<-round(Tair, digits=1)
values_rounded<-unique(Tair_rounded)
survivaldf<-data.frame(values_rounded)
#we calculate corresponding T90 and K from the temperature values
survivaldf$T90_rounded<-temperature1*survivaldf$values_rounded + temperature2
survivaldf$K_rounded<--1*(log(0.1)/survivaldf$T90_rounded)

#get storage time
n<-par$storage_time[i]
nshort<-par$storage_time_low[i]

#add empty columns to fill with survival values
survivaldf$survivallong<-NA
survivaldf$survivalshort<-NA

#loop over the dataframe rows
for (j in 1:length(survivaldf$values_rounded)){
  kk<-survivaldf$K_rounded[j]
 # print(j)
  if(is.na(kk)) next else
    
    f1<- function(t){
      V2<- exp(-kk*t)
      return(V2)
    }
  cc<-integrate(f1, 0, n)
  survivaldf$survivallong[j]<-cc$value/n
  
  ccshort<-integrate(f1, 0, nshort)
  survivaldf$survivalshort[j]<-ccshort$value/nshort
  
 # print(j)
  
}
surv_long <- data.frame(temp=survivaldf$values_rounded, surv=survivaldf$survivallong)
survival <- subs(Tair_rounded, surv_long, subsWithNA=T)

surv_short <- data.frame(temp=survivaldf$values_rounded, surv=survivaldf$survivalshort)
survivalshort <- subs(Tair_rounded, surv_short, subsWithNA=T)


a<-Tair<0
survivalzero<-survival
survivalshortzero<-survivalshort
survivalzero[a]<-0
survivalshortzero[a]<-0

filename="D:/LuciePhD/Model/Cryptomodel/modeldata/survival/storage/survival.asc"
writeRaster(survival,filename=filename,format="ascii",overwrite=TRUE)
filename="D:/LuciePhD/Model/Cryptomodel/modeldata/survival/storage/survivalshort.asc"
writeRaster(survivalshort,filename=filename,format="ascii",overwrite=TRUE)
filename="D:/LuciePhD/Model/Cryptomodel/modeldata/survival/storage/survivalzero.asc"
writeRaster(survivalzero,filename=filename,format="ascii",overwrite=TRUE)
filename="D:/LuciePhD/Model/Cryptomodel/modeldata/survival/storage/survivalshortzero.asc"
writeRaster(survivalshortzero,filename=filename,format="ascii",overwrite=TRUE)


#plot(survival, main= "Survival 9 months storage")
#plot(survivalshort, main="Survival 1 month storage")
#plot(survivalzero, main= "Survival 9 months storage, no survival with average air T below zero")
#plot(survivalshortzero, main= "Survival 1 month storage, no survival with average air T below zero")


###### read in survival rasters
survival<- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/survival/storage/survival.asc")
survivalshort<- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/survival/storage/survivalshort.asc")

Fcattle_survivingstorage <- Fcattle_DSAD + (Fcattle_other*survival)
#plot(Fcattle_survivingstorage, main = "Fcattle surviving storage")
Fbuffaloes_survivingstorage <- Fbuffaloes_DSAD + (Fbuffaloes_other*survival)
#plot(Fbuffaloes_survivingstorage, main = "Fbuffaloes surviving storage")
Fpigs_survivingstorage <- Fpigs_DSAD + (Fpigs_other*survival) + (Fpigs_pitlow*survivalshort)
#plot(Fpigs_survivingstorage, main = "Fpigs surviving storage")
Fpoultry_survivingstorage <- Fpoultry_DSAD + (Fpoultry_other*survival)
#plot(Fpoultry_survivingstorage, main = "Fpoultry surviving storage")
Fsheep_survivingstorage <- Fsheep_DSAD + (Fsheep_other*survival)
#plot(Fsheep_survivingstorage, main = "Fsheep surviving storage")
Fgoats_survivingstorage <- Fgoats_DSAD + (Fgoats_other*survival)
#plot(Fgoats_survivingstorage, main = "Fgoats surviving storage")
Fhorses_survivingstorage <- Fhorses_DSAD + (Fhorses_other*survival)
#plot(Fhorses_survivingstorage, main = "Fhorses surviving storage")
Fasses_survivingstorage <- Fasses_DSAD + (Fasses_other*survival)
#plot(Fasses_survivingstorage, main = "Fasses surviving storage")
Fmules_survivingstorage <- Fmules_DSAD + (Fmules_other*survival)
#plot(Fmules_survivingstorage, main = "Fmules surviving storage")
Fcamels_survivingstorage <- Fcamels_DSAD + (Fcamels_other*survival)
#plot(Fcamels_survivingstorage, main = "Fcamels surviving storage")

#Write raster files for fraction that is still active after storage 
#these still need to be multiplied with the fractions of manure actually going into storage
# this will be done in the script diffuse_emissions.R
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcattle_survivingstorage.asc"
writeRaster(Fcattle_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fbuffaloes_survivingstorage.asc"
writeRaster(Fbuffaloes_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpigs_survivingstorage.asc"
writeRaster(Fpigs_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fpoultry_survivingstorage.asc"
writeRaster(Fpoultry_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fsheep_survivingstorage.asc"
writeRaster(Fsheep_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fgoats_survivingstorage.asc"
writeRaster(Fgoats_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fhorses_survivingstorage.asc"
writeRaster(Fhorses_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fasses_survivingstorage.asc"
writeRaster(Fasses_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fmules_survivingstorage.asc"
writeRaster(Fmules_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)
filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/manure/Fcamels_survivingstorage.asc"
writeRaster(Fcamels_survivingstorage,filename=filename,format="ascii", overwrite=TRUE)


#write.csv(MMSn,file="D:/LuciePhD/Model/Cryptomodel/modeldata/manure/ManureManagementSystemscalculated.csv")



### obsolete ###############
#Read in empty file that will be filled with fractions surviving storage calculated in this script
#FAOS <- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/manure/FAOS.csv")


#### obsolete: doing the survival looping on the raster instead of vector

### NEE, nog beter: dit hele script niet runnen, alleen bij bepaalde sens runs

#if(i=1:...){
#  survival<- raster( ...)
#  survivalshort<-raster(...)
#  
#}else{
  
  # define the time over which to integrate (the standard storage time)
#  n<-par$storage_time[i]
#  nshort<-par$storage_time_low[i]
  
  #create empty raster to store results
#  survival<-K
#  survival[]<-NA
#  survivalshort<-K
#  survivalshort[]<-NA
  
  #loop over K grid cells
  #each time define f1 (the survival function) with K of the corresponding grid cell as parameter
  #then integrate f1 over the period 0:n
  #the survival is $value/n of this integral
  #the same is done for nshort
  
#  for (j in 1:259200){
#    kk<-K[j]
#    print(j)
#    if(is.na(kk)) next else
      
#      f1<- function(t){
#        V2<- exp(-kk*t)
#        return(V2)
#      }
#    cc<-integrate(f1, 0, n)
#    survival[j]<-cc$value/n
#    
#    ccshort<-integrate(f1, 0, nshort)
#    survivalshort[j]<-ccshort$value/nshort
#    
#    print(j)
    
#  }
#} #closing bracket van else statement
