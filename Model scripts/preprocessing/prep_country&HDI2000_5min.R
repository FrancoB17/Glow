#preprocessing country and HDI data for 2000 at 5 min


############################ Fixing states US and China and making HDI on 5 min grid #####

countryfile<-paste(working.path.in,"COUNTRY5.ASC",sep="")
countries5min<-(readAsciiGrid(countryfile))
#States of US and China are all given US country number 840 or China country number 156
a<-which((countries5min$COUNTRY5.ASC > 1500) & (countries5min$COUNTRY5.ASC < 1600))
countries5min$COUNTRY5.ASC[a]<-156
a<-which(countries5min$COUNTRY5.ASC > 3900 & countries5min$COUNTRY5.ASC < 4100)
countries5min$COUNTRY5.ASC[a]<-840


hdi5min <- (readAsciiGrid(countryfile))

b<-0
#Put HDI in correct gridfile
hdi5min[[1]]<-NA
for (i in 1:length(data_table1$hdi)){
  a<-which(countries5min$COUNTRY5.ASC == data_table1$iso[i])
  if (length(a) == 0 || is.na(data_table1$hdi[i])){b<-b+1}else{
    (hdi5min$COUNTRY5.ASC[a]<- data_table1$hdi[i])
  }
}

#Write HDI grid 5 min
fname<-paste(working.path.in,"HDIgrid5min.asc",sep="")
writeAsciiGrid(hdi5min, fname, attr = 1, na.value = -9999)

#Write country grid with states fixed
fname<-paste(working.path.in,"Countries5minNoStates.asc",sep="")
writeAsciiGrid(countries5min, fname, attr = 1, na.value = -9999)

