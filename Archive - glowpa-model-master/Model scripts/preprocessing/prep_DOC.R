# With this script, the Global NEWS dissolved organic carbon basin export values
# are converted for use in the GloWPa model
# written by Lucie in December 2016

globalnews<-read.csv("D:/LuciePhD/Data/GlobalNEWS/c00run5_NEWSInputOutput.csv")

# calculate DOC concentration by dividing exported load (Mg/yr) over total discharge 
# the resulting value is thus in Mg/km3, this is the same as mg/m3
globalnews$DOC_concentration<- globalnews$c0DOCload / globalnews$c0Qnat

#convert to mg/L
globalnews$DOC_concentration_mgL<- globalnews$DOC_concentration / 1000
hist(globalnews$DOC_concentration_mgL)
mediaan<-median(globalnews$DOC_concentration_mgL,na.rm=TRUE)

#read in basin ID map
basinid <- raster("D:/LuciePhD/Model/Cryptomodel/modeldata/BASINID.MAP ")
plot(basinid)

#make new dataframe for making grid
temporary <- data.frame(ID=globalnews$BASINID, value=globalnews$DOC_concentration_mgL)

#subset basinid raster with DOC concentration values
DOCraster <- subs(basinid, temporary, subsWithNA=T)
plot(DOCraster)

#infinite and NaN are produced when discharge or DOC is zero
infinite<-DOCraster == Inf
plot(infinite)
nonumber<-is.nan(DOCraster) 
plot(nonumber)
zero<-DOCraster == 0
plot(zero)

#so we'll set these to the median
DOCraster[infinite]<-mediaan
DOCraster[nonumber]<-mediaan
DOCraster[zero]<-mediaan
unique(DOCraster)

#round values to two decimals
DOCraster_rounded<-round(DOCraster, digits=2)
unique(DOCraster_rounded)
#zeroes<-DOCraster_rounded == 0  # there are no zeroes anymore now
#plot(zeroes, main = "DOC concentration is zero / infinite / NaN")
plot(DOCraster_rounded, main= "DOC concentration (mg/L)")

writeRaster(DOCraster_rounded, filename="D:/LuciePhD/Model/Cryptomodel/modeldata/globalnews/DOCraster.asc", format="ascii", overwrite=TRUE)
