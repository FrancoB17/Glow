rm(list=ls(all=TRUE))

#Path and filenames need to be changed up here and at the bottom of the script.
#Comments are in Dutch, I hope you manage with google translate. Basically, what happens is that first the population in the gridded
#data is summed, so that the total population in a subarea is known. Then the grids are ranked based on population density (pop/area)
#Afterwards, the grids become urban or rural. I do remember that this file did not run for the 1km resolution. It required too much 
#memory. Maybe in case you first cut out China it works, otherwise first aggregate population and the other grids to 10k and then
#use this script. Right now it is set up for 1km.

library(raster)
working.path.in<-"D:/MST/WUR/OneDrive - Wageningen University & Research/PhD/Glowpa_model_human/Preprocess/Population/"

isoraster.china.0.1 <- raster("D:/MST/WUR/OneDrive - Wageningen University & Research/PhD/Glowpa_model_human/Preprocess/Population/isoraster_china0_1.tif")
china.area <- raster::area(isoraster.china.0.1,na.rm = TRUE )
LandArea <- china.area

Population<-raster(paste0(working.path.in,"chinapop01.asc")) #this is the population grid
# LandArea<-raster(paste0(working.path.in,"global_px_area_1km_adaptedextent.tif")) #this is a grid of the area (km2) per 1km grid.
isoraster<- raster("D:/MST/WUR/OneDrive - Wageningen University & Research/PhD/Glowpa_model_human/Preprocess/Population/isoraster_china0_1.tif") #you have this file already
furb<-read.csv(paste0(working.path.in,"input_file_china.csv")) #this file is a csv file with GID, iso3, ID, name and furb  

####what is furb????????????????????????????????????



furb$pop<-NA


library(dplyr)
all_data <- data.frame(Population = values(Population), ISO = values(isoraster))
summed <- all_data %>%
  filter(ISO %in% furb$ID) %>% # restrict to only ISOs in HumanData_iso
  group_by(ISO) %>% # do the following per ISO code
  summarise(PopulationTotal = sum(Population, na.rm = TRUE)) %>%
  ungroup()
furb <- furb[(furb$ID %in% summed$ISO), ]

for (j in 1:length(furb$iso)){
  q<-which(summed$ISO==furb$ID[j])
  if(length(q)>0){
    furb$pop[q]<-summed$PopulationTotal[q]
  }
  print(j)
}

rm(all_data)
rm(summed)

detach("package:dplyr", unload=TRUE)

furb$pop_urb_num<-0
furb$pop_rur_num<-0

furb$pop_urb_num<-furb$pop*furb$furb
furb$pop_rur_num<-furb$pop*(1-furb$furb)

popurban_grid<-Population
urban_grid<-Population
poprural_grid<-Population
population_km2<-Population

population_km2<-Population/LandArea
popurban_grid<-0
urban_grid<-0
poprural_grid<-0

for (i in 1:length(furb$ID)){
  #selecteer gridcellen van een land
  a<-which(isoraster[]==furb$ID[i])

  #maak tabel met lengte aantal gridcellen van een land
  z<-array(dim=length(a))
  #bepaal negatieve populatie per km2 per cel
  pop<--population_km2
  #rank deze cellen, methode min betekent dat dichtstbevolkte de hoogste rank krijgen
  z<-rank(pop,ties.method="min")
  # b is je rank, begin bij 1
  b<-1
  sumgrid<-0
  sumrurgrid<-0
  #wij verdelen furb$pop_urb_num[i]
  if (is.na(furb$pop_urb_num[i])){
    popurban_grid<-0
    poprural_grid<-0
  } else {
    if(furb$pop_urb_num[i]>0){
      for (j in 1:length(a)){
        if (sumgrid<furb$pop_urb_num[i]){
          q<-which(z==b)
          
          if (length(q)>0){
            if(is.na(population_km2[a[q[1]]])){
              b<-b+1
            } else {
              if ((furb$pop_urb_num[i]-(sumgrid+(Population[a[q[1]]]*length(q)))) >= 0){ 
                popurban_grid[a[q]]<-Population[a[q]]
                sumgrid<-sumgrid+sum(popurban_grid[a[q]],na.rm=TRUE)
                b<-b+1
                urban_grid[a[q]]<-1
              } else {
                popurban_grid[a[q]]<-(furb$pop_urb_num[i]-sumgrid)/length(q)
                urban_grid[a[q]]<-2
                poprural_grid[a[q]]<-Population[a[q]]-popurban_grid[a[q]]
                sumgrid<-sumgrid+sum(popurban_grid[a[q]],na.rm=TRUE)
                sumrurgrid<-sumrurgrid+sum(poprural_grid[a[q]],na.rm=TRUE)
                b<-b+1
              }
            }
          } else {
            b<-b+1
          }
        } else {
          if(sumrurgrid<furb$pop_rur_num[i]){
            q<-which(z==b)
            
            if (length(q)>0){
              if(is.na(population_km2[a[q[1]]])){
                b<-b+1
              } else {
                if ((furb$pop_rur_num[i]-(sumrurgrid+(Population[a[q[1]]]*length(q)))) >= 0){ 
                  poprural_grid[a[q]]<-Population[a[q]]
                  sumrurgrid<-sumrurgrid+sum(poprural_grid[a[q]],na.rm=TRUE)
                  b<-b+1
                  urban_grid[a[q]]<-3
                } else {
                  poprural_grid[a[q]]<-(furb$pop_rur_num[i]-sumrurgrid)/length(q)
                  urban_grid[a[q]]<-4
                  break
                }
              }
            } else {
              b<-b+1
            }
          }
        }
      } 
    }else {
      poprural_grid[a]<-0
      popurban_grid[a]<-0
    }
  }
  print(i)
}

writeRaster(poprural_grid,paste0(working.path.in,"pop_rural_world_1km"),format="GTiff",overwrite=TRUE)
writeRaster(popurban_grid,paste0(working.path.in,"pop_urban_world_1km"),format="GTiff",overwrite=TRUE)

isoraster_05<-raster("C:/Users/hofst023/OneDrive - WageningenUR/Oude D schijf/Onderzoek/K2P/test/gadm36_levels/isoraster_level0_05.tif")
poprural_grid_05<-aggregate(poprural_grid,fact=round(res(isoraster_05)/res(poprural_grid)),fun=sum,na.rm=TRUE) 
popurban_grid_05<-aggregate(popurban_grid,fact=round(res(isoraster_05)/res(poprural_grid)),fun=sum,na.rm=TRUE)

isoraster_01<-raster("C:/Users/hofst023/OneDrive - WageningenUR/Oude D schijf/Onderzoek/K2P/test/gadm36_levels/isoraster_level1_01.tif")
poprural_grid_01<-aggregate(poprural_grid,fact=round(res(isoraster_01)/res(poprural_grid)),fun=sum,na.rm=TRUE) 
popurban_grid_01<-aggregate(popurban_grid,fact=round(res(isoraster_01)/res(poprural_grid)),fun=sum,na.rm=TRUE)

writeRaster(poprural_grid_05,paste0(working.path.in,"pop_rural_world_05deg"),format="GTiff",overwrite=TRUE)
writeRaster(popurban_grid_05,paste0(working.path.in,"pop_urban_world_05deg"),format="GTiff",overwrite=TRUE)
writeRaster(poprural_grid_01,paste0(working.path.in,"pop_rural_world_01deg"),format="GTiff",overwrite=TRUE)
writeRaster(popurban_grid_01,paste0(working.path.in,"pop_urban_world_01deg"),format="GTiff",overwrite=TRUE)
