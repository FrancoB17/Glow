# this script computes what part of the diffuse emissions is transported with runoff to rivers
#written by Lucie in March 2017
#updated by Nynke in April 2019 to exclude the animal loads


#read in annual diffuse emission files
#we perform the calculations for animals and humans separately and for both added up
#in this version the animal emissions have been set to 0.
humandiffuse_year<-raster(paste0(working.path.in,"modeloutput_humans/humanemissions_diffuse_",pathogen,i,".asc"))
animaldiffuse_year<-humandiffuse_year
animaldiffuse_year[]<-0
#plot(log10(animaldiffuse_year),main="animal diffuse emissions per year",col=colors)
#plot(log10(humandiffuse_year),main="human diffuse emissions per year",col=colors)

#for now in all months equal oocyst input assumed
#we have to take off some zeroes, otherwise R is not computing correctly
#e.g. totaldiffuse - animaldiffuse - humandiffuse does not give zero
animaldiffuse<-round((animaldiffuse_year*overall_inputs$animal_loads[i]/12)/1e5,digits=0)
humandiffuse<-round((humandiffuse_year*overall_inputs$human_loads[i]/12)/1e5,digits=0) #for now in all months equal oocyst input assumed
#change NA to zero, otherwise adding up goes incorrectly
animaldiffuse[is.na(animaldiffuse)]<-0
humandiffuse[is.na(humandiffuse)]<-0
totaldiffuse<-animaldiffuse+humandiffuse

#Nynke: so now the totaldiffuse only comprises human diffuse 

#totaldiffuselog<-log10(totaldiffuse)
#plot(totaldiffuselog, col=colors2, main="Total diffuse emissions log")

########## read in surface runoff files #######################


raster_data<-paste0(working.path.in,"surfacerunoff/surfacerunoff_",mons,"_",i,".asc")
r_surface<-stack(raster_data)
setwd(wd)

#make sure input and hydrology files have the same extent
humandiffuse<-crop(humandiffuse,extent(isoraster_hydro))
animaldiffuse<-crop(animaldiffuse,extent(isoraster_hydro))
totaldiffuse<-crop(totaldiffuse,extent(isoraster_hydro))
r_surface<-crop(r_surface,extent(isoraster_hydro))

#we need to check the resolution of the loads and the runoff to make sure these are the same
#in case they are not the same, we need to sum the data in the grid that has the highest resolution
#to match the other grid resolution
res_runoff<-res(isoraster_hydro)
res_multiplier<-res_runoff[1]/reso
if(res_multiplier>1){
  humandiffuse<-aggregate(humandiffuse, fact=res_multiplier, fun=sum)
  animaldiffuse<-aggregate(animaldiffuse, fact=res_multiplier, fun=sum)
  totaldiffuse<-aggregate(totaldiffuse, fact=res_multiplier, fun=sum)
}else if (res_multiplier<1){
  print("ERROR: RESOLUTION OF RUNOFF FILES IS HIGHER THAN RESOLUTION OF LOADS") #potentially this could be corrected as well, but I am not sure how and I don't think this model should go below 1km resolution
}

#we have to multiply by 10 and round to zero digits
#because otherwise the subs function later gives erroneous results, don't know why
#probably something to do with how R stores numbers in its memory
r_surface<-r_surface*overall_inputs$runoff[i]*10
r_surface_rounded<-round(r_surface,digits=0)

#the runoff values that actually occur globally range from 0 to 60.8
#however, to be prepared for sensitivity and scenario analyse
#we will make it possible to work with values up to 95 (950, because we multiplied by 10)
#above 95, with retention being 8.8-3.2, we will get negative values
runoff_values2<-seq(from=0,to=950,by=1)
df2<-data.frame(runoff_values2)

#allocate log retention values linearly over this spread
#we fix this relation, so changing runoff gives a change in model outcome
r1<-pathogen_inputs$retention_lower[pathogen_row]
r2<-pathogen_inputs$retention_upper[pathogen_row]

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
split2<-mons #sapply(strsplit(raster_data,"_"), `[`, 2)
filenames2<-paste0(working.path.in,"retention/retention_",split2)
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
filenames<-paste0(working.path.in,"oocysts/diffuse_streaminput_",pathogen,i,split2)
filenames_animal<-paste0(working.path.in,"oocysts/diffuse_streaminput_animal_",pathogen,i,split2)
filenames_human<-paste0(working.path.in,"oocysts/diffuse_streaminput_human_",pathogen,i,split2)
path<-paste0(working.path.in,"oocysts")
setwd(path)
writeRaster(streaminput_rasters, filename=filenames, format="ascii", bylayer=TRUE, overwrite=TRUE)
writeRaster(streaminput_rasters_animal, filename=filenames_animal, format="ascii", bylayer=TRUE, overwrite=TRUE)
writeRaster(streaminput_rasters_human, filename=filenames_human, format="ascii", bylayer=TRUE, overwrite=TRUE)
#plot(streaminput_log10,main=filenames,col=colors2)
setwd(wd)


#sum the diffuse emissions over the subareas and add to the point dataframe. Then save the point dataframe

#To do that, first the raster should be summed over the 12 months (layers)
streaminput_rasters_human_sum<-calc(streaminput_rasters_human, fun=sum,na.rm=TRUE)
writeRaster(streaminput_rasters_human_sum, filename=paste0("diffuse_streaminput_human_annual_",pathogen,i), format="ascii", overwrite=TRUE)

#also open the human emissions to water and add to the streaminput to get the total gridded emissions
human_point<-raster(paste0(working.path.in,"modeloutput_humans/humanemissions_",pathogen,i,".asc"))
human_point<-crop(human_point,extent(isoraster_hydro))
human_point<-raster::aggregate(human_point, fact=res_multiplier, fun=sum)
human_total<-human_point+streaminput_rasters_human_sum
writeRaster(human_total,paste0(working.path.out,"humanemissions_",pathogen,run), format="ascii", overwrite=TRUE)

#Make .png plot

colors<-c("slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")

human_total[human_total==0]<-NA
log_human_total<-log10(human_total)
mx<-max(log_human_total[],na.rm=TRUE)
mn<-min(log_human_total[],na.rm=TRUE)
brks<-c(floor(mn),floor(mn)+(ceiling(mx)-floor(mn))/10,floor(mn)+2*(ceiling(mx)-floor(mn))/10,floor(mn)+3*(ceiling(mx)-floor(mn))/10,floor(mn)+4*(ceiling(mx)-floor(mn))/10,floor(mn)+5*(ceiling(mx)-floor(mn))/10,floor(mn)+6*(ceiling(mx)-floor(mn))/10,floor(mn)+7*(ceiling(mx)-floor(mn))/10,floor(mn)+8*(ceiling(mx)-floor(mn))/10,floor(mn)+9*(ceiling(mx)-floor(mn))/10,ceiling(mx))

#plot Png file
filename_png<-paste0(working.path.out,"humanemissions_",pathogen,run,".png")
png(filename=filename_png,width=750,height=750,units="px")

par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n") 
plot(log_human_total, col=colors, breaks=brks,legend=FALSE, axes=FALSE)
if(isTRUE(scenario_run)){
  title(main=paste0("Human emissions per grid scenario"))  
}else{
  title(main=paste0("Human emissions per grid baseline run"))  
}


#if the resolution is 0.08333 or 0.008333, the district data or higher res need to be plotted, if the resolution is 0.5, the country data needs to be plotted
if(reso == 0.5){
#  plot(countriesHigh, add=TRUE)
}else{
  setwd(paste0(working.path.in,"districts_shapefile/"))
  plot(readOGR(dsn = ".", layer = overall_inputs$shapefile_filename[i]),add=TRUE)
  setwd<-wd
}

if(pathogen=="cryptosporidium"){  
  plot(log_human_total, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=brks,
                      labels=brks, 
                      cex.axis=1,font=1.4),
       legend.args=list(text='Cryptosporidium emissions (log10 oocysts / year)', font=1.8, cex=1.2)) 
}else if(pathogen=="rotavirus"){
  plot(log_human_total, legend.only=TRUE, col=colors,breaks=brks,horizontal=TRUE,
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=brks,
                      labels=brks, 
                      cex.axis=1,font=1.4),
       legend.args=list(text='Rotavirus emissions (log10 viral particles / year)', font=1.8, cex=1.2)) 
}
dev.off()

#end of plotting .png file

writeRaster(human_total,paste0(working.path.out,"humanemissions_",pathogen,run),format="GTiff",overwrite=TRUE)

library(dplyr)
all_data <- data.frame(humandiftowat = values(streaminput_rasters_human_sum),ISO = values(isoraster_hydro))

point$diffuseemissionstowater<-NA

summed <- all_data %>%
  filter(ISO %in% HumanData$iso) %>% # restrict to only ISOs in HumanData_iso
  group_by(ISO) %>% # do the following per ISO code
  summarise(EmissionsTotal = sum(humandiftowat, na.rm = TRUE)) %>%
  ungroup()

for(j in 1:length(HumanData$iso)){ 
  q<-which(summed$ISO==HumanData$iso[j])
  if(length(q>0)){
    point$diffuseemissionstowater[j]<-summed$EmissionsTotal[q]
  }
}

point$diffuseemissionstowaterfromdiffuse_urb<-point$diffuseemissionstowater*point$pathogen_urb_flanddiffuse
point$diffuseemissionstowaterfromdiffuse_rur<-point$diffuseemissionstowater*point$pathogen_rur_flanddiffuse
point$diffuseemissionstowaterfromonsite_urb<-point$diffuseemissionstowater*point$pathogen_urb_flandonsite
point$diffuseemissionstowaterfromonsite_rur<-point$diffuseemissionstowater*point$pathogen_rur_flandonsite

point$pathogen_urb_dif<-point$diffuseemissionstowaterfromdiffuse_urb
point$pathogen_rur_dif<-point$diffuseemissionstowaterfromdiffuse_rur
point$pathogen_urb_onsite_land<-point$diffuseemissionstowaterfromonsite_urb
point$pathogen_rur_onsite_land<-point$diffuseemissionstowaterfromonsite_rur

rm(all_data)
rm(summed)
.rs.unloadPackage("dplyr")

write.csv(point, file=paste0(working.path.out,"HumanEmissionsCalculated_",pathogen,"_",run,".csv"))

rm(animaldiffuse,animaldiffuse_year,humandiffuse,humandiffuse_year,totaldiffuse,streaminput_rasters,streaminput_rasters_animal,streaminput_rasters_human)
rm(raster_data,r_surface_rounded,df2,r1,r2,filenames_animal,filenames_human,r_surface,runoff_values2)
rm(runoff_fraction_rasters,runoff_function,retention_rasters)
rm(path,point,HumanData)


