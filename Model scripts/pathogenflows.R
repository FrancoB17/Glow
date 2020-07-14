library(pathogenflows)
session_info <- sessionInfo()
log_info("Loaded package {session_info$otherPkgs$pathogenflows$Package}  {session_info$otherPkgs$pathogenflows$Version}")

pathogenflow.run <- function(pathogen){
  log_info("Using Pathogenflows to calculate emissions")
  emissions <- data.frame(iso=HUMAN_DATA$iso,subarea=HUMAN_DATA$subarea)
  # 1) extract pathogen type from pathogen
  pathogenType <- pathogen$pathogenType
  area_types <- c("urban","rural")
  human_age_types <- c("child","adult")
  
  all_loadings <- list()
  # 2) call loadings function 4 times for urban-adult/child prevalence and rural-adult/child prevalence
  for(area_type in area_types){
    for(human_age_type in human_age_types ){
      #2a) prepare data for urban and rural
      onsite_data <- pathogenflow.get.input(area_type,human_age_type)
      # file_out <- file.path(SCENARIO$model_input,sprintf("onsite_%s_%s.csv",area_type,human_age_type))
      # write.csv(onsite_data,file = file_out)
      #call loadings
      loadings <- pathogenflows::getLoadings(onsite_data,pathogenType)
      # store in list
      all_loadings[[area_type]][[human_age_type]] <- loadings
    }
  }

  # 3) postprocess output
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
    for(i in 1:length(emissions$subarea)){
      sub_area <- emissions$subarea[i]
      ncols <- ncol(adult[[sub_area]])
      sanitation_types <- adult[[sub_area]]$id
      emissions_sanitation <- adult[[sub_area]][3:ncols] + child[[sub_area]][3:ncols]
      
      # apply to all sanitation types
      for(j in 1:length(sanitation_types)){
        sanitation_type <- sanitation_types[j]
        # construct column names by sanitation type and rural/urban
        emissions_col_name_postfix <- sprintf("%s_%s",sanitation_type,area_type_code)
        emissions_col_to_surface <- sprintf("%s_%s","to_surface",emissions_col_name_postfix)
        emissions_col_sewerage <- sprintf("%s_%s","to_sewerage",emissions_col_name_postfix)
        emissions_col_in_facal_sludge <-  sprintf("%s_%s","to_fecalSludge",emissions_col_name_postfix)
        to_surface <- emissions_sanitation$toSurface[j]
        to_sewerage <- emissions_sanitation$sewerage[j]
        in_fecal_sludge <- emissions_sanitation$fecalSludge[j]
        # store only to_surface, to_sewerage and in_fecal_sludge
        emissions[[emissions_col_to_surface]][i] <- to_surface
        emissions[[emissions_col_sewerage]][i] <- to_sewerage
        emissions[[emissions_col_in_facal_sludge]] <- in_fecal_sludge
      }
    }
  }
  
  HUMAN_DATA$removalfraction<-1-(HUMAN_DATA$FractionPrimarytreatment*pathogen$RemovalPrimary+HUMAN_DATA$FractionSecondarytreatment*pathogen$RemovalSecondary+
                                     HUMAN_DATA$FractionTertiarytreatment*pathogen$RemovalTertiary+HUMAN_DATA$FractionQuaternarytreatment*pathogen$RemovalQuaternary+HUMAN_DATA$FractionPonds*pathogen$RemovalPonds)  
    
  for(i in 1:length(emissions$subarea)){
    emissions$pathogen_urb_conforgrid_sewer[i]<-sum(c(emissions$to_sewerage_flushSewer_urb[i],na.rm=TRUE)) #,emissions$to_sewerage_flushSeptic_urb[i],emissions$to_sewerage_flushPit_urb[i],emissions$to_sewerage_flushOpen_urb[i],emissions$to_sewerage_flushUnknown_urb[i],emissions$to_sewerage_pitSlab_urb[i],emissions$to_sewerage_pitNoSlab_urb[i],emissions$to_sewerage_compostingTwinSlab_urb[i],emissions$to_sewerage_compostingTwinNoSlab_urb[i],emissions$to_sewerage_compostingToilet_urb[i],emissions$to_sewerage_bucketLatrine_urb[i],emissions$to_sewerage_containerBased_urb[i],emissions_to_sewerage_hangingToilet_urb[i],emissions_to_sewerage_openDefecation_urb[i],emissions$to_sewerage_other_urb[i]),na.rm=TRUE) #I added all toilet types, but I think only emissions$to_sewerage_flushSewer_urb should have a value >0
    emissions$pathogen_rur_conforgrid_sewer[i]<-sum(c(emissions$to_sewerage_flushSewer_rur[i],na.rm=TRUE)) #emissions$to_sewerage_flushSeptic_rur[i],emissions$to_sewerage_flushPit_rur[i],emissions$to_sewerage_flushOpen_rur[i],emissions$to_sewerage_flushUnknown_rur[i],emissions$to_sewerage_pitSlab_rur[i],emissions$to_sewerage_pitNoSlab_rur[i],emissions$to_sewerage_compostingTwinSlab_rur[i],emissions$to_sewerage_compostingTwinNoSlab_rur[i],emissions$to_sewerage_compostingToilet_rur[i],emissions$to_sewerage_bucketLatrine_rur[i],emissions$to_sewerage_containerBased_rur[i],emissions_to_sewerage_hangingToilet_rur[i],emissions_to_sewerage_openDefecation_rur[i],emissions$to_sewerage_other_rur[i]),na.rm=TRUE)
    emissions$pathogen_urb_conforgrid_onsite[i]<-sum(c(emissions$to_fecalSludge_flushSeptic_urb[i],emissions$to_fecalSludge_flushPit_urb[i],emissions$to_fecalSludge_flushOpen_urb[i],emissions$to_fecalSludge_flushUnknown_urb[i],emissions$to_fecalSludge_pitSlab_urb[i],emissions$to_fecalSludge_pitNoSlab_urb[i],emissions$to_fecalSludge_compostingTwinSlab_urb[i],emissions$to_fecalSludge_compostingTwinNoSlab_urb[i],emissions$to_fecalSludge_compostingToilet_urb[i],emissions$to_fecalSludge_bucketLatrine_urb[i],emissions$to_fecalSludge_containerBased_urb[i],emissions$to_fecalSludge_other_urb[i]),na.rm=TRUE) #emissions$to_fecalSludge_flushSewer_urb[i],,emissions_to_fecalSludge_hangingToilet_urb[i],emissions_to_fecalSludge_openDefecation_urb[i]
    emissions$pathogen_rur_conforgrid_onsite[i]<-sum(c(emissions$to_fecalSludge_flushSeptic_rur[i],emissions$to_fecalSludge_flushPit_rur[i],emissions$to_fecalSludge_flushOpen_rur[i],emissions$to_fecalSludge_flushUnknown_rur[i],emissions$to_fecalSludge_pitSlab_rur[i],emissions$to_fecalSludge_pitNoSlab_rur[i],emissions$to_fecalSludge_compostingTwinSlab_rur[i],emissions$to_fecalSludge_compostingTwinNoSlab_rur[i],emissions$to_fecalSludge_compostingToilet_rur[i],emissions$to_fecalSludge_bucketLatrine_rur[i],emissions$to_fecalSludge_containerBased_rur[i],emissions$to_fecalSludge_other_rur[i]),na.rm=TRUE) #emissions$to_fecalSludge_flushSewer_rur[i],,emissions_to_fecalSludge_hangingToilet_rur[i],emissions_to_fecalSludge_openDefecation_rur[i]
    
    if(SCENARIO$wwtp_available==3){
      emissions$pathogen_urb_conforgrid[i]<-emissions$pathogen_urb_conforgrid_sewer[i]+emissions$pathogen_urb_conforgrid_onsite[i]
      emissions$pathogen_rur_conforgrid[i]<-emissions$pathogen_rur_conforgrid_sewer[i]+emissions$pathogen_rur_conforgrid_onsite[i]
      emissions$pathogen_urb_waterforgrid[i]<-sum(c(emissions$to_surface_flushSewer_urb[i],emissions$to_surface_flushSeptic_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_flushPit_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_flushOpen_urb[i],emissions$to_surface_flushUnknown_urb[i],emissions$to_surface_pitSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_pitNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingToilet_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_bucketLatrine_urb[i],emissions$to_surface_containerBased_urb[i],emissions$to_surface_hangingToilet_urb[i],emissions$to_surface_openDefecation_urb[i],emissions$to_surface_other_urb[i]),na.rm=TRUE)
      emissions$pathogen_rur_waterforgrid[i]<-sum(c(emissions$to_surface_flushSewer_rur[i],emissions$to_surface_flushSeptic_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_flushPit_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_flushOpen_rur[i],emissions$to_surface_flushUnknown_rur[i],emissions$to_surface_pitSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_pitNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingToilet_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_hangingToilet_rur[i],emissions$to_surface_other_rur[i]),na.rm=TRUE)
      
      for(j in length(sanitation_types)){
        jmp_type <- sanitation_types[j]
        col_name_urb <- sprintf("pathogen_urb_%s",jmp_type) 
        col_name_rur <- sprintf("pathogen_rur_%s",jmp_type)
        emissions[[col_name_urb]] <- 0
        emissions[[col_name_rur]] <- 0
      }
      
      a<-which(emissions$pathogen_urb_conforgrid>0)
      b<-which(emissions$pathogen_rur_conforgrid>0)

      emissions$pathogen_urb_fconsewer[a]<-emissions$pathogen_urb_conforgrid_sewer[a]/emissions$pathogen_urb_conforgrid[a]
      emissions$pathogen_rur_fconsewer[b]<-emissions$pathogen_rur_conforgrid_sewer[b]/emissions$pathogen_rur_conforgrid[b]
      emissions$pathogen_urb_fonsite_flushSeptic[a]<-emissions$to_fecalSludge_flushSeptic_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_flushSeptic[b]<-emissions$to_fecalSludge_flushSeptic_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_flushPit[a]<-emissions$to_fecalSludge_flushPit_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_flushPit[b]<-emissions$to_fecalSludge_flushPit_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_flushOpen[a]<-emissions$to_fecalSludge_flushOpen_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_flushOpen[b]<-emissions$to_fecalSludge_flushOpen_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_flushUnknown[a]<-emissions$to_fecalSludge_flushUnknown_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_flushUnknown[b]<-emissions$to_fecalSludge_flushUnknown_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_pitSlab[a]<-emissions$to_fecalSludge_pitSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_pitSlab[b]<-emissions$to_fecalSludge_pitSlab_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_pitNoSlab[a]<-emissions$to_fecalSludge_pitNoSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_pitNoSlab[b]<-emissions$to_fecalSludge_pitNoSlab_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_compostingTwinSlab[a]<-emissions$to_fecalSludge_compostingTwinSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_compostingTwinSlab[b]<-emissions$to_fecalSludge_compostingTwinSlab_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_compostingTwinNoSlab[a]<-emissions$to_fecalSludge_compostingTwinNoSlab_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_compostingTwinNoSlab[b]<-emissions$to_fecalSludge_compostingTwinNoSlab_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_compostingToilet[a]<-emissions$to_fecalSludge_compostingToilet_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_compostingToilet[b]<-emissions$to_fecalSludge_compostingToilet_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_bucketLatrine[a]<-emissions$to_fecalSludge_bucketLatrine_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_bucketLatrine[b]<-emissions$to_fecalSludge_bucketLatrine_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_containerBased[a]<-emissions$to_fecalSludge_containerBased_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_containerBased[b]<-emissions$to_fecalSludge_containerBased_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
      emissions$pathogen_urb_fonsite_other[a]<-emissions$to_fecalSludge_other_urb[a]/(emissions$pathogen_urb_conforgrid[a]*(1-emissions$pathogen_urb_fconsewer[a]))
      emissions$pathogen_rur_fonsite_other[b]<-emissions$to_fecalSludge_other_rur[b]/(emissions$pathogen_urb_conforgrid[b]*(1-emissions$pathogen_rur_fconsewer[b]))
    } else{
      emissions$pathogen_urb_conforgrid[i]<-NA
      emissions$pathogen_rur_conforgrid[i]<-NA
      emissions$pathogen_urb_waterforgrid[i]<-(emissions$pathogen_urb_conforgrid_sewer[i]+emissions$pathogen_urb_conforgrid_onsite[i])*HUMAN_DATA$removalfraction[i]+sum(c(emissions$to_surface_flushSewer_urb[i],emissions$to_surface_flushSeptic_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_flushPit_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_flushOpen_urb[i],emissions$to_surface_flushUnknown_urb[i],emissions$to_surface_pitSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_pitNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinNoSlab_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_compostingToilet_urb[i]*(1-HUMAN_DATA$onsiteDumpedland_urb[i]),emissions$to_surface_bucketLatrine_urb[i],emissions$to_surface_containerBased_urb[i],emissions$to_surface_hangingToilet_urb[i],emissions$to_surface_openDefecation_urb[i],emissions$to_surface_other_urb[i]),na.rm=TRUE)
      emissions$pathogen_rur_waterforgrid[i]<-(emissions$pathogen_rur_conforgrid_sewer[i]+emissions$pathogen_rur_conforgrid_onsite[i])*HUMAN_DATA$removalfraction[i]+sum(c(emissions$to_surface_flushSewer_rur[i],emissions$to_surface_flushSeptic_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_flushPit_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_flushOpen_rur[i],emissions$to_surface_flushUnknown_rur[i],emissions$to_surface_pitSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_pitNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinNoSlab_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_compostingToilet_rur[i]*(1-HUMAN_DATA$onsiteDumpedland_rur[i]),emissions$to_surface_hangingToilet_rur[i],emissions$to_surface_other_rur[i]),na.rm=TRUE)
      emissions$to_sewerage_flushSewer_urb_out[i]<-emissions$pathogen_urb_conforgrid_sewer[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_sewerage_flushSewer_rur_out[i]<-emissions$pathogen_rur_conforgrid_sewer[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushSeptic_urb_out[i]<-emissions$to_fecalSludge_flushSeptic_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushPit_urb_out[i]<-emissions$to_fecalSludge_flushPit_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushOpen_urb_out[i]<-emissions$to_fecalSludge_flushOpen_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_flushUnknown_urb_out[i]<-emissions$to_fecalSludge_flushUnknown_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_pitSlab_urb_out[i]<-emissions$to_fecalSludge_pitSlab_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_pitNoSlab_urb_out[i]<-emissions$to_fecalSludge_pitNoSlab_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_compostingTwinSlab_urb_out[i]<-emissions$to_fecalSludge_compostingTwinSlab_urb[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_compostingTwinNoSlab_urb_out[i]<-emissions$to_fecalSludge_compostingTwinNoSlab_urb[i]*HUMAN_DATA$removalfraction[i]
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
      emissions$to_fecalSludge_compostingTwinSlab_rur_out[i]<-emissions$to_fecalSludge_compostingTwinSlab_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_compostingTwinNoSlab_rur_out[i]<-emissions$to_fecalSludge_compostingTwinNoSlab_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_compostingToilet_rur_out[i]<-emissions$to_fecalSludge_compostingToilet_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_bucketLatrine_rur_out[i]<-emissions$to_fecalSludge_bucketLatrine_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_containerBased_rur_out[i]<-emissions$to_fecalSludge_containerBased_rur[i]*HUMAN_DATA$removalfraction[i]
      emissions$to_fecalSludge_other_rur_out[i]<-emissions$to_fecalSludge_other_rur[i]*HUMAN_DATA$removalfraction[i]
    }
    
    emissions$pathogen_urb_landforgrid[i]<-sum(c(emissions$to_surface_flushSeptic_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_flushPit_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_pitSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_pitNoSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_compostingTwinSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_compostingTwinNoSlab_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i],emissions$to_surface_compostingToilet_urb[i]*HUMAN_DATA$onsiteDumpedland_urb[i]),na.rm=TRUE)
    emissions$pathogen_rur_landforgrid[i]<-sum(c(emissions$to_surface_flushSeptic_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_flushPit_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_pitSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_pitNoSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_compostingTwinSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_compostingTwinNoSlab_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_compostingToilet_rur[i]*HUMAN_DATA$onsiteDumpedland_rur[i],emissions$to_surface_bucketLatrine_rur[i],emissions$to_surface_bucketLatrine_rur[i]),na.rm=TRUE)
  }
    
  emissions$to_surface_flushSewer_urb_out<-emissions$to_surface_flushSewer_urb
  emissions$to_surface_flushSeptic_urb_out<-emissions$to_surface_flushSeptic_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_flushSeptic_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_flushPit_urb_out<-emissions$to_surface_flushPit_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_flushPit_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_flushOpen_urb_out<-emissions$to_surface_flushOpen_urb
  emissions$to_surface_flushUnknown_urb_out<-emissions$to_surface_flushUnknown_urb
  emissions$to_surface_pitSlab_urb_out<-emissions$to_surface_pitSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_pitSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_pitNoSlab_urb_out<-emissions$to_surface_pitNoSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_pitNoSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_compostingTwinSlab_urb_out<-emissions$to_surface_compostingTwinSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_compostingTwinSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
  emissions$to_surface_compostingTwinNoSlab_urb_out<-emissions$to_surface_compostingTwinNoSlab_urb*(1-HUMAN_DATA$onsiteDumpedland_urb)+emissions$to_surface_compostingTwinNoSlab_urb*HUMAN_DATA$onsiteDumpedland_urb*0.025
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
  emissions$to_surface_compostingTwinSlab_rur_out<-emissions$to_surface_compostingTwinSlab_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_compostingTwinSlab_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
  emissions$to_surface_compostingTwinNoSlab_rur_out<-emissions$to_surface_compostingTwinNoSlab_rur*(1-HUMAN_DATA$onsiteDumpedland_rur)+emissions$to_surface_compostingTwinNoSlab_rur*HUMAN_DATA$onsiteDumpedland_rur*0.025
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

pathogenflow.get.input <- function(area_type,human_type){
  #@Nynke: population is population per human type (adult/child) or total population?
  col_names <- c("iso","scenario","region","flushSewer","flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine",
                 "containerBased","hangingToilet","openDefecation","other","coverBury","sewageTreated","fecalSludgeTreated","isWatertight","hasLeach",
                 "emptyFrequency", "pitAdditive", "urine")  
  onsite_data <- data.frame(matrix(ncol=length(col_names),nrow = dim(HUMAN_DATA)[1]))
  colnames(onsite_data) <- col_names
  onsite_data$iso <- HUMAN_DATA$iso
  onsite_data$region <- HUMAN_DATA$subarea
  onsite_data$sheddingRate <- HUMAN_DATA$sheddingRate
  
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
    onsite_data$population <- HUMAN_DATA$population * HUMAN_DATA$fraction_pop_under5
    if(area_type == "urban"){
      onsite_data$population <- onsite_data$population * HUMAN_DATA$fraction_urban_pop
      incidence <-HUMAN_DATA$incidence_urban_under5
    }
    else if(area_type == "rural"){
      onsite_data$population <- onsite_data$population * (1 - HUMAN_DATA$fraction_urban_pop)
      incidence <- HUMAN_DATA$incidence_rural_under5
    }
  }
  else if(human_type == "adult"){
    onsite_data$population <- HUMAN_DATA$population * (1-HUMAN_DATA$fraction_pop_under5)
    if(area_type == "urban"){
      onsite_data$population <- onsite_data$population * HUMAN_DATA$fraction_urban_pop
      incidence <-HUMAN_DATA$incidence_urban_5plus
    }
    else if(area_type == "rural"){
      onsite_data$population <- onsite_data$population * (1 - HUMAN_DATA$fraction_urban_pop)
      incidence <- HUMAN_DATA$incidence_rural_5plus
    }
  }
  
  onsite_data$excreted <- onsite_data$population * HUMAN_DATA$sheddingRate * HUMAN_DATA$shedding_duration
  return(onsite_data)
}

pathogenflow.wwtp <- function(emissions){
  log_info("Applying WWTP for pathogenflow emissions ")
  for(area_type in c("urb","rur")){
    emissions[[sprintf("to_sewerage_flushSewer_%s_out",area_type)]] <- emissions[[sprintf("pathogen_%s_WWTP_sewer",area_type)]]
    for(jmp_type in c("flushSeptic","flushPit","flushOpen","flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine","containerBased","other")){
      print(jmp_type)
      col_name_target <- sprintf("to_fecalSludge_%s_%s_out",jmp_type,area_type)
      col_name_source <- sprintf("pathogen_%s_WWTP_onsite",area_type)
      col_name_factor <- sprintf("pathogen_%s_fonsite_%s",area_type,jmp_type)
      emissions[[col_name_target]] <- emissions[[col_name_source]] * emissions[[col_name_factor]]
    } 
  }
  return(emissions)
}

pathogenflow.calc.totals <- function(emissions){
  browser()
  log_info("Calculating totals emissions per jmp sanition types")
  totals <- data.frame(matrix(nrow=nrow(emissions),ncol=0))
  totals$iso <- emissions$iso
  totals$subarea <- emissions$subarea
  totals$total_flushSewer_out<-emissions$to_sewerage_flushSewer_urb_out+ emissions$to_surface_flushSewer_urb_out+ emissions$to_sewerage_flushSewer_rur_out+ emissions$to_surface_flushSewer_rur_out
  totals$total_flushSeptic_out<-emissions$to_fecalSludge_flushSeptic_urb_out+emissions$to_surface_flushSeptic_urb_out+emissions$to_fecalSludge_flushSeptic_rur_out+emissions$to_surface_flushSeptic_rur_out
  totals$total_flushPit_out<-emissions$to_fecalSludge_flushPit_urb_out+emissions$to_surface_flushPit_urb_out+emissions$to_fecalSludge_flushPit_rur_out+emissions$to_surface_flushPit_rur_out
  totals$total_flushOpen_out<-emissions$to_fecalSludge_flushOpen_urb_out+emissions$to_surface_flushOpen_urb_out+emissions$to_fecalSludge_flushOpen_rur_out+emissions$to_surface_flushOpen_rur_out
  totals$total_flushUnknown_out<-emissions$to_fecalSludge_flushUnknown_urb_out+emissions$to_surface_flushUnknown_urb_out+emissions$to_fecalSludge_flushUnknown_rur_out+emissions$to_surface_flushUnknown_rur_out
  totals$total_pitSlab_out<-emissions$to_fecalSludge_pitSlab_urb_out+emissions$to_surface_pitSlab_urb_out+emissions$to_fecalSludge_pitSlab_rur_out+emissions$to_surface_pitSlab_rur_out
  totals$total_pitNoSlab_out<-emissions$to_fecalSludge_pitNoSlab_urb_out+emissions$to_surface_pitNoSlab_urb_out+emissions$to_fecalSludge_pitNoSlab_rur_out+emissions$to_surface_pitNoSlab_rur_out
  totals$total_compostingTwinSlab_out<-emissions$to_fecalSludge_compostingTwinSlab_urb_out+emissions$to_surface_compostingTwinSlab_urb_out+emissions$to_fecalSludge_compostingTwinSlab_rur_out+emissions$to_surface_compostingTwinSlab_rur_out
  totals$total_compostingTwinNoSlab_out<-emissions$to_fecalSludge_compostingTwinNoSlab_urb_out+emissions$to_surface_compostingTwinNoSlab_urb_out+emissions$to_fecalSludge_compostingTwinNoSlab_rur_out+emissions$to_surface_compostingTwinNoSlab_rur_out
  totals$total_compostingToilet_out<-emissions$to_fecalSludge_compostingToilet_urb_out+emissions$to_surface_compostingToilet_urb_out+emissions$to_fecalSludge_compostingToilet_rur_out+emissions$to_surface_compostingToilet_rur_out
  totals$total_bucketLatrine_out<-emissions$to_fecalSludge_bucketLatrine_urb_out+emissions$to_surface_bucketLatrine_urb_out+emissions$to_fecalSludge_bucketLatrine_rur_out+emissions$to_surface_bucketLatrine_rur_out
  totals$total_containerBased_out<-emissions$to_fecalSludge_containerBased_urb_out+emissions$to_surface_containerBased_urb_out+emissions$to_fecalSludge_containerBased_rur_out+emissions$to_surface_containerBased_rur_out
  totals$total_hangingToilet_out<-emissions$to_surface_hangingToilet_urb_out+emissions$to_surface_hangingToilet_rur_out
  totals$total_openDefecation_out<-emissions$to_surface_openDefecation_urb_out+emissions$to_surface_openDefecation_rur_out
  totals$total_other_out<-emissions$to_fecalSludge_other_urb_out+emissions$to_surface_other_urb_out+emissions$to_fecalSludge_other_rur_out+emissions$to_surface_other_rur_out
  
  return(totals)
}