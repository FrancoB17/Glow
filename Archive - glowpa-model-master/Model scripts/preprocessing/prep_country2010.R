# The file countries30min2010 assigns grid cells to countries based on numeric ISO codes.
#This file was created by Lucie on 13-08-2015 using the following script:


data(gridCountriesDegreesHalf)               #country iso codes
countries<-raster(gridCountriesDegreesHalf)
# codes die wel op de kaart voorkomen, maar niet in de animals data file:
# -99, 248, 729, 728,  260, 239, 10
# ignore 10 (Antarctica) and 248, 239, 260 (small island states)

#fix Western Sahara, Kosovo and Somalia Puntland (appear as -99 on map)
f<-countries%in%-99
#plot(f)
kosovo<-extent(10,30,35,55)
#plot(f, ext=kosovo)
wsahara<-extent(-25,-5,15,35)
#plot(f,ext=wsahara)
puntland<-extent(35,55,0,20)
#plot(f, ext=puntland)
r1 <- crop(f, extent(kosovo))
r2 <- crop(f, extent(wsahara))
r3 <- crop(f, extent(puntland))
countries[r1]<- 688
countries[r2]<-732
countries[r3]<-706
f<-countries%in%-99
plot(f)
plot(countries)

#fix Sudan (appears as two countries with different codes on map, sudan and south sudan)
g<- countries%in%c(729, 728)
countries[g]<-736
rm(f,r1,r2,r3, kosovo, puntland, wsahara,g)

filename<-"D:/LuciePhD/Model/Cryptomodel/modeldata/countries30min2010.grd"
writeRaster(countries, filename=filename, format= "raster")