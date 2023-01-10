# this script computes what part of the diffuse emissions is transported with runoff to rivers
#written by Lucie in March 2017

#read in annual diffuse emission files
#we perform the calculations for animals and humans separately and for both added up
animaldiffuse_year<-raster(paste0("D:/LuciePhD/Model/Cryptomodel/modeloutput_livestock/",parc$diffuse_filename[i]))
humandiffuse_year<-raster("D:/LuciePhD/Model/Cryptomodel/modeloutput_humans/humanemissions_diffuse.asc")
#plot(log10(animaldiffuse_year),main="animal diffuse emissions per year",col=colors)
#plot(log10(humandiffuse_year),main="human diffuse emissions per year",col=colors)

#for now in all months equal oocyst input assumed
#we have to take off some zeroes, otherwise R is not computing correctly
#e.g. totaldiffuse - animaldiffuse - humandiffuse does not give zero
animaldiffuse<-round((animaldiffuse_year*parc$animal_loads[i]/12)/1e5,digits=0)
humandiffuse<-round((humandiffuse_year*parc$human_loads[i]/12)/1e5,digits=0) #for now in all months equal oocyst input assumed
#change NA to zero, otherwise adding up goes incorrectly
animaldiffuse[is.na(animaldiffuse)]<-0
humandiffuse[is.na(humandiffuse)]<-0
totaldiffuse<-animaldiffuse+humandiffuse

#totaldiffuselog<-log10(totaldiffuse)
#plot(totaldiffuselog, col=colors2, main="Total diffuse emissions log")

########## read in surface runoff files #######################
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/surfacerunoff")
raster_data <- list.files(path="D:/LuciePhD/Model/Cryptomodel/modeldata/WaterCycle/VIC/processed/surfacerunoff") 
r_surface<-stack(raster_data)
#we have to multiply by 10 and round to zero digits
#because otherwise the subs function later gives erroneous results, don't know why
#probably something to do with how R stores numbers in its memory
r_surface<-r_surface*parc$runoff[i]*10
r_surface_rounded<-round(r_surface,digits=0)

#the runoff values that actually occur range from 0 to 60.8
#however, to be prepared for sensitivity and scenario analyse
#we will make it possible to work with values up to 95 (950, because we multiplied by 10)
#above 95, with retention being 8.8-3.2, we will get negative values
runoff_values2<-seq(from=0,to=950,by=1)
df2<-data.frame(runoff_values2)

#allocate log retention values linearly over this spread
#we fix this relation, so changing runoff gives a change in model outcome
r1<-parc$retention_lower[i]
r2<-parc$retention_upper[i]

for(jj in 1:length(runoff_values2)){
  if(df2$runoff_values2[jj] <= 200){
    df2$retention[jj]<-r2-((df2$runoff_values2[jj]/200)*(r2-r1))
  }
  if(df2$runoff_values2[jj] > 200){
    df2$retention[jj]<-r1-(((df2$runoff_values2[jj]-200)/400)*(r1-1))
  }
  if(df2$retention[jj] < 1){
    df2$retention[jj]<-1
  }
}

#change zero runoff to NA
#because at zero runoff, retention is infinite, no oocysts are transported
df2$retention[1]<-NA

#creation retention rasters by substituting runoff values with corresponding retention
retention_rasters<-subs(r_surface_rounded,df2,by="runoff_values2",which="retention")

#to check how many months a certain location does not have surface runoff
#r_surface_zeroes<-r_surface_rounded==0
#zeroes<-sum(r_surface_zeroes)
#plot(zeroes,col=colors2)
#to check if all runoff cells have gotten a retention value:
#r_surface_test<-r_surface_rounded
#r_surface_test[r_surface_zeroes]<-NA
#runoffNA<-is.na(r_surface_test)
#retentionNA<-is.na(retention_rasters)
#sumtest<-runoffNA+retentionNA

# it looks like most grid cells have quite low surface runoff values, 
# so most will get the upper end of the retention
split2<-sapply(strsplit(raster_data,"_"), `[`, 2)
filenames2<-paste0("retention_",split2)
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/retention")
writeRaster(retention_rasters, filename=filenames2, format="ascii", bylayer=TRUE, overwrite=TRUE)
#plot(retention_rasters, main= filenames2)

#read this in to start from here
#retention_files<- list.files(path="D:/LuciePhD/Model/Cryptomodel/modeldata/retention") 
#setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/retention")
#retention_rasters<-stack(retention_files)

#define runoff fraction function
runoff_function<-function(x){
     y<-10^(-x)
    return (y)
  }

#apply function and visualize
runoff_fraction_rasters<-calc(retention_rasters,fun=runoff_function)
#plot(runoff_fraction_rasters, main="runoff fraction",col=colors2)
#runoff_fraction_log10<-log10(runoff_fraction_rasters)
#plot(runoff_fraction_log10, main="log10 runoff fraction",col=colors2)


#multiply diffuse emissions with runoff fraction rasters
#in this section: multiply by 1e5 again
streaminput_rasters<-round(totaldiffuse*runoff_fraction_rasters*1e5,digits=0)
#plot(streaminput_rasters, main="streaminput rasters",col=colors2)
#streaminput_log10<-log10(streaminput_rasters)
#plot(streaminput_log10,main="log10 streaminput rasters",col=colors2)
#same for animals only
streaminput_rasters_animal<-round(animaldiffuse*runoff_fraction_rasters*1e5, digits=0)
#plot(streaminput_rasters_animal, main="streaminput rasters animal",col=colors2)
#streaminput_log10_animal<-log10(streaminput_rasters_animal)
#plot(streaminput_log10_animal,main="log10 streaminput rasters animal",col=colors2)
#same for humans only
streaminput_rasters_human<-round(humandiffuse*runoff_fraction_rasters*1e5,digits=0)
#plot(streaminput_rasters_human, main="streaminput rasters human",col=colors2)
#streaminput_log10_human<-log10(streaminput_rasters_human)
#plot(streaminput_log10_human,main="log10 streaminput rasters human",col=colors2)

#define filenames and write rasters
#split2<-sapply(strsplit(retention_files,"_"), `[`, 2)
filenames<-paste0("diffuse_streaminput_",i,split2)
filenames_animal<-paste0("diffuse_streaminput_animal_",i,split2)
filenames_human<-paste0("diffuse_streaminput_human_",i,split2)
setwd("D:/LuciePhD/Model/Cryptomodel/modeldata/oocysts")
writeRaster(streaminput_rasters, filename=filenames, format="ascii", bylayer=TRUE, overwrite=TRUE)
writeRaster(streaminput_rasters_animal, filename=filenames_animal, format="ascii", bylayer=TRUE, overwrite=TRUE)
writeRaster(streaminput_rasters_human, filename=filenames_human, format="ascii", bylayer=TRUE, overwrite=TRUE)
#plot(streaminput_log10,main=filenames,col=colors2)

rm(animaldiffuse,animaldiffuse_year,humandiffuse,humandiffuse_year,totaldiffuse,streaminput_rasters,streaminput_rasters_animal,streaminput_rasters_human)
rm(raster_data,r_surface_rounded,df2,r1,r2,filenames_animal,filenames_human,r_surface,runoff_values2)
rm(runoff_fraction_rasters,runoff_function,retention_rasters)


