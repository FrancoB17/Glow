library(pathogenflows)

session_info <- sessionInfo()
log_info("Loaded package {session_info$otherPkgs$pathogenflows$Package}  {session_info$otherPkgs$pathogenflows$Version}")

pathogenflow.run <- function(pathogen){
  log_info("Using Pathogenflows to calculate emissions")
  emissions <- data.frame(iso=HUMAN_DATA$iso,subarea=HUMAN_DATA$subarea)
  # 1) extract pathogen type from pathogen
  pathogen_type <- pathogen$pathogenType
  area_types <- c("urban","rural")
  human_age_types <- c("child","adult")
  
  all_loadings <- list(rural=list(),urban=list())
  # 2) call loadings function 4 times for urban-adult/child prevalence and rural-adult/child prevalence
  onsite_data <- list(urban=list(),rural=list())
  for(area_type in area_types){
    for(human_age_type in human_age_types){
      onsite_data[[area_type]][[human_age_type]] <- pathogenflow.get.input(area_type,human_age_type,pathogen_type)
    }
  }

  tic("calc pathogenflows loadings")
  # for debugging inside getLoadings
  # loadings <- pathogenflows::getLoadings(onsite_data$urban$child,pathogen_type)
  cl <- makeCluster(detectCores())
  loadings <- parLapply(cl,unlist(onsite_data,recursive = F),fun = function(x){
    out <- pathogenflows::getLoadings(x,pathogen_type)
    return(out)
  })
  stopCluster(cl)
  
  toc(log = T)
  # 3) postprocess output
  for(calc_id in names(loadings)){
    calc_combination <- unlist(strsplit(calc_id,split='.',fixed = T))
    area_type <- calc_combination[1]
    age_type <- calc_combination[2]
    all_loadings[[area_type]][[age_type]] <- loadings[[calc_id]]
  }
  
# uncomment for saving intermediate results  
# for(i in names(unlist(all_loadings, recursive = F))){
##   browser()
#   write.csv(unlist(all_loadings,recursive = F)[[i]]$output,file.path(SCENARIO$model_output,sprintf("loadings_%s_%s_%s.csv",i,PATHOGEN$name,SCENARIO$run)))
# }
  
  # add child  + adult loadings for sanitation and areas
  # apply to area types urban/rural
  for(area_type in names(all_loadings)){
    area_type_code <- "rur"
    if(area_type == "urban"){
      area_type_code <- "urb"
    }
    area_loadings <- all_loadings[[area_type]]
    adult <- area_loadings$adult$det
    child <- area_loadings$child$det
    
    # apply to all sub areas
    for(i in 1:length(emissions$iso)){
      iso_code <- emissions$iso[i]
      gid <- HUMAN_DATA$gid[HUMAN_DATA$iso==iso_code]
      ncols <- ncol(adult[[gid]])
      sanitation_types <- adult[[gid]]$id
      emissions_sanitation <- adult[[gid]][3:ncols] + child[[gid]][3:ncols]

      # apply to all sanitation types
      for(j in 1:length(sanitation_types)){
        sanitation_type <- sanitation_types[j]
        # construct column names by sanitation type and rural/urban
        emissions_col_name_postfix <- sprintf("%s_%s",sanitation_type,area_type_code)
        emissions_col_to_surface <- sprintf("%s_%s","to_surface",emissions_col_name_postfix)
        emissions_col_sewerage <- sprintf("%s_%s","to_sewerage",emissions_col_name_postfix)
        emissions_col_in_fecal_sludge <-  sprintf("%s_%s","to_fecalSludge",emissions_col_name_postfix)
        to_surface <- emissions_sanitation$toSurface[j]
        to_sewerage <- emissions_sanitation$sewerage[j]
        in_fecal_sludge <- emissions_sanitation$fecalSludge[j]
        # store only to_surface, to_sewerage and in_fecal_sludge
        emissions[[emissions_col_to_surface]][i] <- to_surface
        emissions[[emissions_col_sewerage]][i] <- to_sewerage
        emissions[[emissions_col_in_fecal_sludge]][i] <- in_fecal_sludge
      }
    }
  }
  
  removal_fraction_col <- sprintf("fEmitted_inEffluent_after_treatment_%s",tolower(pathogen_type))
  if(removal_fraction_col %in% colnames(HUMAN_DATA)){
    # TODO: check @Nynke 1- this column or just this column
    HUMAN_DATA$removalfraction <<- HUMAN_DATA[[removal_fraction_col]]
  }
  else{
    HUMAN_DATA$removalfraction<-HUMAN_DATA$fEmitted_inEffluent_after_treatment 
  }
  # TODO: check with @Nynke. Can we ussume this?
  HUMAN_DATA$removalfraction <- replace(HUMAN_DATA$removalfraction, is.na(HUMAN_DATA$removalfraction),1)
  
  for(i in 1:length(emissions$subarea)){
    emissions$pathogen_urb_conforgrid_sewer[i]<-sum(c(emissions$to_sewerage_flushSewer_urb[i]),na.rm=TRUE) #,emissions$to_sewerage_flushSeptic_urb[i],emissions$to_sewerage_flushPit_urb[i],emissions$to_sewerage_flushOpen_urb[i],emissions$to_sewerage_flushUnknown_urb[i],emissions$to_sewerage_pitSlab_urb[i],emissions$to_sewerage_pitNoSlab_urb[i],emissions$to_sewerage_compostingTwinSlab_urb[i],emissions$to_sewerage_compostingTwinNoSlab_urb[i],emissions$to_sewerage_compostingToilet_urb[i],emissions$to_sewerage_bucketLatrine_urb[i],emissions$to_sewerage_containerBased_urb[i],emissions_to_sewerage_hangingToilet_urb[i],emissions_to_sewerage_openDefecation_urb[i],emissions$to_sewerage_other_urb[i]),na.rm=TRUE) #I added all toilet types, but I think only emissions$to_sewerage_flushSewer_urb should have a value >0
    emissions$pathogen_rur_conforgrid_sewer[i]<-sum(c(emissions$to_sewerage_flushSewer_rur[i]),na.rm=TRUE) #emissions$to_sewerage_flushSeptic_rur[i],emissions$to_sewerage_flushPit_rur[i],emissions$to_sewerage_flushOpen_rur[i],emissions$to_sewerage_flushUnknown_rur[i],emissions$to_sewerage_pitSlab_rur[i],emissions$to_sewerage_pitNoSlab_rur[i],emissions$to_sewerage_compostingTwinSlab_rur[i],emissions$to_sewerage_compostingTwinNoSlab_rur[i],emissions$to_sewerage_compostingToilet_rur[i],emissions$to_sewerage_bucketLatrine_rur[i],emissions$to_sewerage_containerBased_rur[i],emissions_to_sewerage_hangingToilet_rur[i],emissions_to_sewerage_openDefecation_rur[i],emissions$to_sewerage_other_rur[i]),na.rm=TRUE)
    emissions$pathogen_urb_conforgrid_onsite[i]<-sum(c(emissions$to_fecalSludge_flushSeptic_urb[i],emissions$to_fecalSludge_flushPit_urb[i],emissions$to_fecalSludge_flushOpen_urb[i],emissions$to_fecalSludge_flushUnknown_urb[i],emissions$to_fecalSludge_pitSlab_urb[i],emissions$to_fecalSludge_pitNoSlab_urb[i],emissions$to_fecalSludge_compostingToilet_urb[i],emissions$to_fecalSludge_bucketLatrine_urb[i],emissions$to_fecalSludge_containerBased_urb[i],emissions$to_fecalSludge_other_urb[i]),na.rm=TRUE) #emissions$to_fecalSludge_flushSewer_urb[i],,emissions_to_fecalSludge_hangingToilet_urb[i],emissions_to_fecalSludge_openDefecation_urb[i]
    emissions$pathogen_rur_conforgrid_onsite[i]<-sum(c(emissions$to_fecalSludge_flushSeptic_rur[i],emissions$to_fecalSludge_flushPit_rur[i],emissions$to_fecalSludge_flushOpen_rur[i],emissions$to_fecalSludge_flushUnknown_rur[i],emissions$to_fecalSludge_pitSlab_rur[i],emissions$to_fecalSludge_pitNoSlab_rur[i],emissions$to_fecalSludge_compostingToilet_rur[i],emissions$to_fecalSludge_bucketLatrine_rur[i],emissions$to_fecalSludge_containerBased_rur[i],emissions$to_fecalSludge_other_rur[i]),na.rm=TRUE) #emissions$to_fecalSludge_flushSewer_rur[i],,emissions_to_fecalSludge_hangingToilet_rur[i],emissions_to_fecalSludge_openDefecation_rur[i]
    
    if(SCENARIO$wwtp_available==3){
      emissions$pathogen_urb_conforgrid[i]<-emissions$pathogen_urb_conforgrid_sewer[i]+emissions$pathogen_urb_conforgrid_onsite[i]
      emissions$pathogen_rur_conforgrid[i]<-emissions$pathogen_rur_conforgrid_sewer[i]+emissions$pathogen_rur_conforgrid_onsite[i]
      emissions$pathogen_urb_waterforgrid[i]<-sum(c(emissions$to_surface_flushSewer_urb[i],emissions$to_surface_flushSeptic_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_flushPit_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_flushOpen_urb[i],emissions$to_surface_flushUnknown_urb[i],emissions$to_surface_pitSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_pitNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingToilet_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_bucketLatrine_urb[i],emissions$to_surface_containerBased_urb[i],emissions$to_surface_hangingToilet_urb[i],emissions$to_surface_openDefecation_urb[i],emissions$to_surface_other_urb[i]),na.rm=TRUE)
      emissions$pathogen_rur_waterforgrid[i]<-sum(c(emissions$to_surface_flushSewer_rur[i],emissions$to_surface_flushSeptic_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_flushPit_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_flushOpen_rur[i],emissions$to_surface_flushUnknown_rur[i],emissions$to_surface_pitSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_pitNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingToilet_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_hangingToilet_rur[i],emissions$to_surface_other_rur[i]),na.rm=TRUE)
    }
    #MAYBE BETTER TO MOVE THIS TO LATER AND REPLACE BY THE 'OUT' EMISSIONS, currently then calculating twice the same thing.
    else{
      emissions$pathogen_urb_conforgrid[i]<-NA
      emissions$pathogen_rur_conforgrid[i]<-NA
      emissions$pathogen_urb_waterforgrid[i]<-sum(emissions$pathogen_urb_conforgrid_sewer[i],emissions$pathogen_urb_conforgrid_onsite[i], na.rm = T)*HUMAN_DATA$removalfraction[i]+
        sum(c(emissions$to_surface_flushSewer_urb[i],
              emissions$to_surface_flushSeptic_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              emissions$to_surface_flushPit_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              emissions$to_surface_flushOpen_urb[i],emissions$to_surface_flushUnknown_urb[i],
              emissions$to_surface_pitSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              emissions$to_surface_pitNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              # emissions$to_surface_compostingTwinSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              # emissions$to_surface_compostingTwinNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              emissions$to_surface_compostingToilet_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),
              emissions$to_surface_bucketLatrine_urb[i],emissions$to_surface_containerBased_urb[i],
              emissions$to_surface_hangingToilet_urb[i],emissions$to_surface_openDefecation_urb[i],
              emissions$to_surface_other_urb[i]),na.rm=TRUE)
      emissions$pathogen_rur_waterforgrid[i]<-sum(emissions$pathogen_rur_conforgrid_sewer[i],emissions$pathogen_rur_conforgrid_onsite[i], na.rm = T)*HUMAN_DATA$removalfraction[i]+
        sum(c(emissions$to_surface_flushSewer_rur[i],
              emissions$to_surface_flushSeptic_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              emissions$to_surface_flushPit_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              emissions$to_surface_flushOpen_rur[i],
              emissions$to_surface_flushUnknown_rur[i],
              emissions$to_surface_pitSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              emissions$to_surface_pitNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              # emissions$to_surface_compostingTwinSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              # emissions$to_surface_compostingTwinNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              emissions$to_surface_compostingToilet_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),
              emissions$to_surface_hangingToilet_rur[i],
              emissions$to_surface_other_rur[i]),na.rm=TRUE)
      emissions$to_sewerage_flushSewer_urb_out[i]<-emissions$pathogen_urb_conforgrid_sewer[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_sewerage_flushSewer_rur_out[i]<-emissions$pathogen_rur_conforgrid_sewer[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushSeptic_urb_out[i]<-emissions$to_fecalSludge_flushSeptic_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushPit_urb_out[i]<-emissions$to_fecalSludge_flushPit_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushOpen_urb_out[i]<-emissions$to_fecalSludge_flushOpen_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushUnknown_urb_out[i]<-emissions$to_fecalSludge_flushUnknown_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_pitSlab_urb_out[i]<-emissions$to_fecalSludge_pitSlab_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_pitNoSlab_urb_out[i]<-emissions$to_fecalSludge_pitNoSlab_urb[i]*HUMAN_DATA$removalfraction[i]
#      emissions$to_fecalSludge_compostingTwinSlab_urb_out[i]<-emissions$to_fecalSludge_compostingTwinSlab_urb[i]*HUMAN_DATA$removalfraction[i]
#      emissions$to_fecalSludge_compostingTwinNoSlab_urb_out[i]<-emissions$to_fecalSludge_compostingTwinNoSlab_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_compostingToilet_urb_out[i]<-emissions$to_fecalSludge_compostingToilet_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_bucketLatrine_urb_out[i]<-emissions$to_fecalSludge_bucketLatrine_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_containerBased_urb_out[i]<-emissions$to_fecalSludge_containerBased_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_other_urb_out[i]<-emissions$to_fecalSludge_other_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushSeptic_rur_out[i]<-emissions$to_fecalSludge_flushSeptic_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushPit_rur_out[i]<-emissions$to_fecalSludge_flushPit_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushOpen_rur_out[i]<-emissions$to_fecalSludge_flushOpen_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushUnknown_rur_out[i]<-emissions$to_fecalSludge_flushUnknown_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_pitSlab_rur_out[i]<-emissions$to_fecalSludge_pitSlab_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_pitNoSlab_rur_out[i]<-emissions$to_fecalSludge_pitNoSlab_rur[i]*HUMAN_DATA$removalfraction[i]
#      emissions$to_fecalSludge_compostingTwinSlab_rur_out[i]<-emissions$to_fecalSludge_compostingTwinSlab_rur[i]*HUMAN_DATA$removalfraction[i]
#      emissions$to_fecalSludge_compostingTwinNoSlab_rur_out[i]<-emissions$to_fecalSludge_compostingTwinNoSlab_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_compostingToilet_rur_out[i]<-emissions$to_fecalSludge_compostingToilet_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_bucketLatrine_rur_out[i]<-emissions$to_fecalSludge_bucketLatrine_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_containerBased_rur_out[i]<-emissions$to_fecalSludge_containerBased_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_other_rur_out[i]<-emissions$to_fecalSludge_other_rur[i]*HUMAN_DATA$removalfraction[i]
    }

    emissions$pathogen_urb_landforgrid[i]<-sum(c(emissions$to_surface_flushSeptic_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_flushPit_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_pitSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_pitNoSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_compostingToilet_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i]),na.rm=TRUE) #emissions$to_surface_compostingTwinSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_compostingTwinNoSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],
    emissions$pathogen_rur_landforgrid[i]<-sum(c(emissions$to_surface_flushSeptic_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_flushPit_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_pitSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_pitNoSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_compostingToilet_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_bucketLatrine_rur[i],emissions$to_surface_containerBased_rur[i],emissions$to_surface_openDefecation_rur[i]),na.rm=TRUE) #,emissions$to_surface_compostingTwinSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_compostingTwinNoSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i]
  }
  
  for(j in 1:length(sanitation_types)){

    jmp_type <- sanitation_types[j]
    col_name_urb <- sprintf("pathogen_urb_fonsite_%s",jmp_type) 
    col_name_rur <- sprintf("pathogen_rur_fonsite_%s",jmp_type)
    emissions[[col_name_urb]] <- 0
    emissions[[col_name_rur]] <- 0
  }
  emissions$pathogen_urb_fconsewer <- 0
  emissions$pathogen_rur_fconsewer <- 0
      
  a<-which(emissions$pathogen_urb_conforgrid>0)
  b<-which(emissions$pathogen_rur_conforgrid>0)

  if(length(a)>0){
    emissions$pathogen_urb_fconsewer[a]<-emissions$pathogen_urb_conforgrid_sewer[a]/emissions$pathogen_urb_conforgrid[a]
    emissions$pathogen_urb_fonsite_flushSeptic[a]<-emissions$to_fecalSludge_flushSeptic_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_flushPit[a]<-emissions$to_fecalSludge_flushPit_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_flushOpen[a]<-emissions$to_fecalSludge_flushOpen_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_flushUnknown[a]<-emissions$to_fecalSludge_flushUnknown_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_pitSlab[a]<-emissions$to_fecalSludge_pitSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_pitNoSlab[a]<-emissions$to_fecalSludge_pitNoSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    #emissions$pathogen_urb_fonsite_compostingTwinSlab[a]<-emissions$to_fecalSludge_compostingTwinSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    #emissions$pathogen_urb_fonsite_compostingTwinNoSlab[a]<-emissions$to_fecalSludge_compostingTwinNoSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_compostingToilet[a]<-emissions$to_fecalSludge_compostingToilet_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_bucketLatrine[a]<-emissions$to_fecalSludge_bucketLatrine_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_containerBased[a]<-emissions$to_fecalSludge_containerBased_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
    emissions$pathogen_urb_fonsite_other[a]<-emissions$to_fecalSludge_other_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
  }
  if(length(b) >0){
    emissions$pathogen_rur_fconsewer[b]<-emissions$pathogen_rur_conforgrid_sewer[b]/emissions$pathogen_rur_conforgrid[b]
    emissions$pathogen_rur_fonsite_flushSeptic[b]<-emissions$to_fecalSludge_flushSeptic_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_flushPit[b]<-emissions$to_fecalSludge_flushPit_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_flushOpen[b]<-emissions$to_fecalSludge_flushOpen_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_flushUnknown[b]<-emissions$to_fecalSludge_flushUnknown_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_pitSlab[b]<-emissions$to_fecalSludge_pitSlab_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_pitNoSlab[b]<-emissions$to_fecalSludge_pitNoSlab_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    #emissions$pathogen_rur_fonsite_compostingTwinSlab[b]<-emissions$to_fecalSludge_compostingTwinSlab_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    #emissions$pathogen_rur_fonsite_compostingTwinNoSlab[b]<-emissions$to_fecalSludge_compostingTwinNoSlab_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_compostingToilet[b]<-emissions$to_fecalSludge_compostingToilet_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_bucketLatrine[b]<-emissions$to_fecalSludge_bucketLatrine_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_containerBased[b]<-emissions$to_fecalSludge_containerBased_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    emissions$pathogen_rur_fonsite_other[b]<-emissions$to_fecalSludge_other_rur[b]/(emissions$pathogen_rur_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
  }
  
  emissions$to_surface_flushSewer_urb_out<-emissions$to_surface_flushSewer_urb
  emissions$to_surface_flushSeptic_urb_out<-emissions$to_surface_flushSeptic_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_flushSeptic_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_flushPit_urb_out<-emissions$to_surface_flushPit_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_flushPit_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_flushOpen_urb_out<-emissions$to_surface_flushOpen_urb
  emissions$to_surface_flushUnknown_urb_out<-emissions$to_surface_flushUnknown_urb
  emissions$to_surface_pitSlab_urb_out<-emissions$to_surface_pitSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_pitSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_pitNoSlab_urb_out<-emissions$to_surface_pitNoSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_pitNoSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  #emissions$to_surface_compostingTwinSlab_urb_out<-emissions$to_surface_compostingTwinSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_compostingTwinSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  #emissions$to_surface_compostingTwinNoSlab_urb_out<-emissions$to_surface_compostingTwinNoSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_compostingTwinNoSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_compostingToilet_urb_out<-emissions$to_surface_compostingToilet_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_compostingToilet_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_bucketLatrine_urb_out<-emissions$to_surface_bucketLatrine_urb
  emissions$to_surface_containerBased_urb_out<-emissions$to_surface_containerBased_urb
  emissions$to_surface_hangingToilet_urb_out<-emissions$to_surface_hangingToilet_urb
  emissions$to_surface_openDefecation_urb_out<-emissions$to_surface_openDefecation_urb
  emissions$to_surface_other_urb_out<-emissions$to_surface_other_urb
  
  emissions$to_surface_flushSewer_rur_out<-emissions$to_surface_flushSewer_rur
  emissions$to_surface_flushSeptic_rur_out<-emissions$to_surface_flushSeptic_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_flushSeptic_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  emissions$to_surface_flushPit_rur_out<-emissions$to_surface_flushPit_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_flushPit_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  emissions$to_surface_flushOpen_rur_out<-emissions$to_surface_flushOpen_rur
  emissions$to_surface_flushUnknown_rur_out<-emissions$to_surface_flushUnknown_rur
  emissions$to_surface_pitSlab_rur_out<-emissions$to_surface_pitSlab_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_pitSlab_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  emissions$to_surface_pitNoSlab_rur_out<-emissions$to_surface_pitNoSlab_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_pitNoSlab_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  #emissions$to_surface_compostingTwinSlab_rur_out<-emissions$to_surface_compostingTwinSlab_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_compostingTwinSlab_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  #emissions$to_surface_compostingTwinNoSlab_rur_out<-emissions$to_surface_compostingTwinNoSlab_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_compostingTwinNoSlab_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  emissions$to_surface_compostingToilet_rur_out<-emissions$to_surface_compostingToilet_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_compostingToilet_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  emissions$to_surface_bucketLatrine_rur_out<-emissions$to_surface_bucketLatrine_rur*0.025
  emissions$to_surface_containerBased_rur_out<-emissions$to_surface_containerBased_rur*0.025
  emissions$to_surface_hangingToilet_rur_out<-emissions$to_surface_hangingToilet_rur
  emissions$to_surface_openDefecation_rur_out<-emissions$to_surface_openDefecation_rur*0.025
  emissions$to_surface_other_rur_out<-emissions$to_surface_other_rur
    
  #WAT NU NOG MIST IS VOOR IEDERE TOILET CATEGORIE URB EN RUR EEN ARRAY EMISSIONS$TO_SEWARAGE_XXX_XXX_OUT
  #EN EMISSIONS$TO_FECALSLUDGE_XXX_XXX_OUT. DAT KAN NA HET TREATMENT STUK GEMAAKT WORDEN
  #DAN MOETEN DE EMISSIONS$TO_SURFACE, EMISSIONS$TO_SEWERAGE EN EMISSIONS$TO_FECAL_SLUDGE OPGETELD
  #WORDEN TOT TOTALE EMISSIONS PER TOILET TYPE voor urbaan + ruraal (BIJV EMISSIONS$TOTAL_XXX__OUT) DIE DAN KUNNEN WORDEN
  #UITGESCHREVEN ALS JSON
  
  # 4) return emissions
  return(emissions)
}

pathogenflow.get.input <- function(area_type,human_type, pathogen_type){
  #@Nynke: population is population per human type (adult/child) or total population?
  col_names <- c("gid","scenario","region","flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine",
                 "containerBased","hangingToilet","openDefecation","other","coverBury","sewageTreated","fecalSludgeTreated","isWatertight","hasLeach",
                 "emptyFrequency", "pitAdditive", "urine","twinPits")  
  onsite_data <- data.frame(matrix(ncol=length(col_names),nrow = dim(HUMAN_DATA)[1]))
  colnames(onsite_data) <- col_names
  onsite_data$gid <- HUMAN_DATA$gid
  onsite_data$region <- HUMAN_DATA$subarea
  shedding_rate_col <- sprintf("sheddingRate_%s", tolower(pathogen_type))
  if(shedding_rate_col %in% colnames(HUMAN_DATA)){
    onsite_data$sheddingRate <- HUMAN_DATA[[shedding_rate_col]]
  }
  else{
    onsite_data$sheddingRate <- HUMAN_DATA$sheddingRate
  }
  # set shedding duration for pathogen type
  shedding_duration_col <- sprintf("shedding_duration_%s",tolower(pathogen_type))
  if(shedding_duration_col %in% colnames(HUMAN_DATA)){
    HUMAN_DATA$shedding_duration <<- HUMAN_DATA[[shedding_duration_col]]
  }
  input_col_names <- colnames(HUMAN_DATA)
  if(area_type=="urban"){
    postfix <- "urb"
    onsite_data$population <- HUMAN_DATA$population*HUMAN_DATA$fraction_urban_pop
  }
  else if(area_type=="rural"){
    postfix <- "rur"
    onsite_data$population <- HUMAN_DATA$population* (1- HUMAN_DATA$fraction_urban_pop)
  }
  # filter for urban and rural
  filtered_cols <- input_col_names[endsWith(input_col_names,postfix)]
  for(input_colname in filtered_cols){
    # get corresponding colname for onsite_data
    onsite_colname <- gsub(sprintf("_%s",postfix),"",input_colname)
    # check if it part of onsite_data
    if(onsite_colname %in% col_names){
      onsite_data[[onsite_colname]] <- HUMAN_DATA[[input_colname]]
    }
  }
  # voor kinderen under 5 urbaan: population*fraction_urban_pop*fraction_pop_under5*incidence_urban_under5*sheddingRate*sheddingDuration
  # voor kinderen under 5 ruraal: population*(1-fraction_urban_pop)*fraction_pop_under5*incidence_rural_under5*sheddingRate*sheddingDuration 
  # voor mensen>5 urbaan: population*fraction_urban_pop*(1-fraction_pop_under5)*incidence_urban_5plus*sheddingRate*sheddingDuration 
  # voor mensen>5 ruraal: population*(1-fraction_urban_pop)*(1-fraction_pop_under5)*incidence_rural_5plus*sheddingRate*sheddingDuration
  incidence <- 0
  # set population and incidence
  if(human_type == "child"){
    onsite_data$population <- onsite_data$population * HUMAN_DATA$fraction_pop_under5
    age_post_fix <- "under5"
  }
  else if(human_type == "adult"){
    onsite_data$population <- onsite_data$population * (1-HUMAN_DATA$fraction_pop_under5)
    age_post_fix <- "5plus"
  }
  # find the right incidence for urban/child and pathogen type
  incidence_col_name <- sprintf("incidence_%s_%s",area_type,age_post_fix)
  if(sprintf("%s_%s",incidence_col_name,tolower(pathogen_type)) %in% colnames(HUMAN_DATA)){
    incidence_col_name <- sprintf("%s_%s",incidence_col_name,tolower(pathogen_type))
  }
  incidence <- HUMAN_DATA[[incidence_col_name]]
  onsite_data$excreted <- onsite_data$population * onsite_data$sheddingRate * HUMAN_DATA$shedding_duration * incidence
  return(onsite_data)
}

pathogenflow.wwtp <- function(emissions){
  log_info("Applying WWTP for pathogenflow emissions ")
  for(area_type in c("urb","rur")){
    emissions[[sprintf("to_sewerage_flushSewer_%s_out",area_type)]] <- emissions[[sprintf("pathogen_%s_WWTP_sewer",area_type)]]
    for(jmp_type in c("flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingToilet","bucketLatrine","containerBased","other")){
      col_name_target <- sprintf("to_fecalSludge_%s_%s_out",jmp_type,area_type)
      col_name_source <- sprintf("pathogen_%s_WWTP_onsite",area_type)
      col_name_factor <- sprintf("pathogen_%s_fonsite_%s",area_type,jmp_type)
      emissions[[col_name_target]] <- emissions[[col_name_source]] * emissions[[col_name_factor]]
    } 
  }
  return(emissions)
}

pathogenflow.calc.totals <- function(emissions){
  log_info("Calculating totals emissions per jmp sanition types")
  totals <- data.frame(matrix(nrow=nrow(emissions),ncol=0))
  totals$iso <- emissions$iso
  totals$subarea <- emissions$subarea
  totals$total_flushSewer_out<- rowSums(cbind(emissions$to_sewerage_flushSewer_urb_out, emissions$to_surface_flushSewer_urb_out, emissions$to_sewerage_flushSewer_rur_out, emissions$to_surface_flushSewer_rur_out),na.rm=T)
  totals$total_flushSeptic_out<- rowSums(cbind(emissions$to_fecalSludge_flushSeptic_urb_out,emissions$to_surface_flushSeptic_urb_out,emissions$to_fecalSludge_flushSeptic_rur_out,emissions$to_surface_flushSeptic_rur_out), na.rm=T)
  totals$total_flushPit_out<- rowSums(cbind(emissions$to_fecalSludge_flushPit_urb_out,emissions$to_surface_flushPit_urb_out,emissions$to_fecalSludge_flushPit_rur_out,emissions$to_surface_flushPit_rur_out), na.rm=T)
  totals$total_flushOpen_out<- rowSums(cbind(emissions$to_fecalSludge_flushOpen_urb_out,emissions$to_surface_flushOpen_urb_out,emissions$to_fecalSludge_flushOpen_rur_out,emissions$to_surface_flushOpen_rur_out), na.rm=T)
  totals$total_flushUnknown_out<-rowSums(cbind(emissions$to_fecalSludge_flushUnknown_urb_out,emissions$to_surface_flushUnknown_urb_out,emissions$to_fecalSludge_flushUnknown_rur_out,emissions$to_surface_flushUnknown_rur_out), na.rm=T)
  totals$total_pitSlab_out<- rowSums(cbind(emissions$to_fecalSludge_pitSlab_urb_out,emissions$to_surface_pitSlab_urb_out,emissions$to_fecalSludge_pitSlab_rur_out,emissions$to_surface_pitSlab_rur_out), na.rm = T)
  totals$total_pitNoSlab_out<-rowSums(cbind(emissions$to_fecalSludge_pitNoSlab_urb_out,emissions$to_surface_pitNoSlab_urb_out,emissions$to_fecalSludge_pitNoSlab_rur_out,emissions$to_surface_pitNoSlab_rur_out), na.rm=T)
  #totals$total_compostingTwinSlab_out<- emissions$to_fecalSludge_compostingTwinSlab_urb_out,emissions$to_surface_compostingTwinSlab_urb_out,emissions$to_fecalSludge_compostingTwinSlab_rur_out,emissions$to_surface_compostingTwinSlab_rur_out
  #totals$total_compostingTwinNoSlab_out<- emissions$to_fecalSludge_compostingTwinNoSlab_urb_out,emissions$to_surface_compostingTwinNoSlab_urb_out,emissions$to_fecalSludge_compostingTwinNoSlab_rur_out,emissions$to_surface_compostingTwinNoSlab_rur_out
  totals$total_compostingToilet_out<- rowSums(cbind(emissions$to_fecalSludge_compostingToilet_urb_out,emissions$to_surface_compostingToilet_urb_out,emissions$to_fecalSludge_compostingToilet_rur_out,emissions$to_surface_compostingToilet_rur_out), na.rm = T)
  totals$total_bucketLatrine_out<- rowSums(cbind(emissions$to_fecalSludge_bucketLatrine_urb_out,emissions$to_surface_bucketLatrine_urb_out,emissions$to_fecalSludge_bucketLatrine_rur_out,emissions$to_surface_bucketLatrine_rur_out), na.rm=T)
  totals$total_containerBased_out<- rowSums(cbind(emissions$to_fecalSludge_containerBased_urb_out,emissions$to_surface_containerBased_urb_out,emissions$to_fecalSludge_containerBased_rur_out,emissions$to_surface_containerBased_rur_out), na.rm = T)
  totals$total_hangingToilet_out<- rowSums(cbind(emissions$to_surface_hangingToilet_urb_out,emissions$to_surface_hangingToilet_rur_out), na.rm = T)
  totals$total_openDefecation_out<- rowSums(cbind(emissions$to_surface_openDefecation_urb_out,emissions$to_surface_openDefecation_rur_out), na.rm = T)
  totals$total_other_out<- rowSums(cbind(emissions$to_fecalSludge_other_urb_out,emissions$to_surface_other_urb_out,emissions$to_fecalSludge_other_rur_out,emissions$to_surface_other_rur_out), na.rm = T)

  #THERE IS A MISTAKE IN THE CODE BELOW, pitNoSlab seems to have been excluded from the totals calculateion.  
  for (i in 1:length(HUMAN_DATA$iso)){
    if(sum(as.numeric(totals[i,3:15]),na.rm=TRUE)==0 && sum(as.numeric(HUMAN_DATA[i,21:33]),na.rm=TRUE)+sum(as.numeric(HUMAN_DATA[i,44:56]),na.rm=TRUE)==0){
      totals[i,3:15]<-NA      
    }
  }
  
  totals$total<-NA
  for(i in 1:length(totals$iso)){
    totals$total[i]<-sum(c(totals$total_flushSewer_out[i],totals$total_flushSeptic_out[i],totals$total_flushPit_out[i],totals$total_flushOpen_out[i],totals$total_flushUnknown_out[i],totals$total_pitSlab_out[i],totals$total_flushpitNoSlab_out[i],totals$total_compostingToilet_out[i],totals$total_bucketLatrine_out[i],totals$total_containerBased_out[i],totals$total_hangingToilet_out[i],totals$total_openDefecation_out[i],totals$total_other_out[i]),na.rm=TRUE)
  }
  
  for(i in 1:length(totals$iso)){
    if(sum(as.numeric(totals[i,3:15]),na.rm=TRUE)==0 && sum(as.numeric(HUMAN_DATA[i,21:33]),na.rm=TRUE)+sum(as.numeric(HUMAN_DATA[i,44:56]),na.rm=TRUE)==0){
      totals$total[i]<-NA   
    }  
  }
  
  totals$total1<-log10(totals$total)
  a<-which(totals$total1=="-Inf")
  totals$total1[a]<-0
  totals$total_log<-totals$total1
  
  totals$total1<-NULL
  
  return(totals)
}

