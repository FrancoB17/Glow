library(pathogenflows)

pathogenflow.run <- function(pathogen){
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
      onsite_data <- pathogenflow.model.get.input(area_type,human_age_type)
      file_out <- file.path(working.path.in,sprintf("onsite_%s_%s.csv",area_type,human_age_type))
      write.csv(onsite_data,file = file_out)
      #call loadings
      loadings <- pathogenflows::getLoadings(file_out,pathogenType)
      # store in list
      all_loadings[[area_type]][[human_age_type]] <- loadings
    }
  }
  
  # 3) postprocess output
  # add child  + adult loadings for sanitation and areas
  # apply to area types urban/rural
  for(area_type in names(all_loadings)){
    area_loadings <- all_loadings[[area_type]]
    adult <- area_loadings$adult$detailed
    child <- area_loadings$child$detailed
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
        emissions_col_name_postfix <- sprintf("%s_%s",sanitation_type,area_type)
        emissions_col_to_surface <- sprintf("%s_%s","to_surface",emissions_col_name_postfix)
        emissions_col_sewerage <- sprintf("%s_%s","sewerage",emissions_col_name_postfix)
        emissions_col_in_facal_sludge <-  sprintf("%s_%s","in_fecal_sludge",emissions_col_name_postfix)
        to_surface <- emissions_sanitation$toSurface[j]
        to_sewerage <- emissions_sanitation$sewerage[j]
        in_fecal_sludge <- emissions_sanitation$In_Fecal_Sludge[j]
        # store only to_surface, to_sewerage and in_fecal_sludge
        emissions[[emissions_col_to_surface]][i] <- to_surface
        emissions[[emissions_col_sewerage]][i] <- to_sewerage
        emissions[[emissions_col_in_facal_sludge]] <- in_fecal_sludge
      }
    }
    
    HUMAN$removalfraction<-1-(HUMAN_DATA$treatm_1*pathogen_inputs$RemovalPrimary[pathogen_row]+HUMAN_DATA$treatm_2*pathogen_inputs$RemovalSecondary[pathogen_row]+HUMAN_DATA$treatm_3*pathogen_inputs$RemovalTertiary[pathogen_row]+HUMAN_DATA$treatm_4*pathogen_inputs$RemovalQuaternary[pathogen_row]+HUMAN_DATA$treatm_ponds*pathogen_inputs$RemovalPonds[pathogen_row])  
    
    for(i in length(emission$subarea)){
      emissions$pathogen_urb_conforgrid_sewer[i]<-sum(c(emissions$to_sewerage_flushSewer_urb[i],emissions$to_sewerage_flushSeptic_urb[i],emissions$to_sewerage_flushPit_urb[i],emissions$to_sewerage_flushOpen_urb[i],emissions$to_sewerage_flushUnknown_urb[i],emissions$to_sewerage_pitSlab_urb[i],emissions$to_sewerage_pitNoSlab_urb[i],emissions$to_sewerage_compostingTwinSlab_urb[i],emissions$to_sewerage_compostingTwinNoSlab_urb[i],emissions$to_sewerage_compostingToilet_urb[i],emissions$to_sewerage_bucketLatrine_urb[i],emissions$to_sewerage_containerBased_urb[i],emissions_to_sewerage_hangingToilet_urb[i],emissions_to_sewerage_openDefecation_urb[i],emissions$to_sewerage_other_urb[i]),na.rm=TRUE) #I added all toilet types, but I think only emissions$to_sewerage_flushSewer_urb should have a value >0
      emissions$pathogen_rur_conforgrid_sewer[i]<-sum(c(emissions$to_sewerage_flushSewer_rur[i],emissions$to_sewerage_flushSeptic_rur[i],emissions$to_sewerage_flushPit_rur[i],emissions$to_sewerage_flushOpen_rur[i],emissions$to_sewerage_flushUnknown_rur[i],emissions$to_sewerage_pitSlab_rur[i],emissions$to_sewerage_pitNoSlab_rur[i],emissions$to_sewerage_compostingTwinSlab_rur[i],emissions$to_sewerage_compostingTwinNoSlab_rur[i],emissions$to_sewerage_compostingToilet_rur[i],emissions$to_sewerage_bucketLatrine_rur[i],emissions$to_sewerage_containerBased_rur[i],emissions_to_sewerage_hangingToilet_rur[i],emissions_to_sewerage_openDefecation_rur[i],emissions$to_sewerage_other_rur[i]),na.rm=TRUE)
      emissions$pathogen_urb_conforgrid_onsite[i]<-sum(c(emissions$to_fecalSludge_flushSewer_urb[i],emissions$to_fecalSludge_flushSeptic_urb[i],emissions$to_fecalSludge_flushPit_urb[i],emissions$to_fecalSludge_flushOpen_urb[i],emissions$to_fecalSludge_flushUnknown_urb[i],emissions$to_fecalSludge_pitSlab_urb[i],emissions$to_fecalSludge_pitNoSlab_urb[i],emissions$to_fecalSludge_compostingTwinSlab_urb[i],emissions$to_fecalSludge_compostingTwinNoSlab_urb[i],emissions$to_fecalSludge_compostingToilet_urb[i],emissions$to_fecalSludge_bucketLatrine_urb[i],emissions$to_fecalSludge_containerBased_urb[i],emissions_to_fecalSludge_hangingToilet_urb[i],emissions_to_fecalSludge_openDefecation_urb[i],emissions$to_fecalSludge_other_urb[i]),na.rm=TRUE)
      emissions$pathogen_rur_conforgrid_onsite[i]<-sum(c(emissions$to_fecalSludge_flushSewer_rur[i],emissions$to_fecalSludge_flushSeptic_rur[i],emissions$to_fecalSludge_flushPit_rur[i],emissions$to_fecalSludge_flushOpen_rur[i],emissions$to_fecalSludge_flushUnknown_rur[i],emissions$to_fecalSludge_pitSlab_rur[i],emissions$to_fecalSludge_pitNoSlab_rur[i],emissions$to_fecalSludge_compostingTwinSlab_rur[i],emissions$to_fecalSludge_compostingTwinNoSlab_rur[i],emissions$to_fecalSludge_compostingToilet_rur[i],emissions$to_fecalSludge_bucketLatrine_rur[i],emissions$to_fecalSludge_containerBased_rur[i],emissions_to_fecalSludge_hangingToilet_rur[i],emissions_to_fecalSludge_openDefecation_rur[i],emissions$to_fecalSludge_other_rur[i]),na.rm=TRUE)
      
      if(wwtp_available==3){
        emissions$pathogen_urb_conforgrid[i]<-emissions$pathogen_urb_conforgrid_sewer[i]+emissions$pathogen_urb_conforgrid_onsite[i]
        emissions$pathogen_rur_conforgrid[i]<-emissions$pathogen_rur_conforgrid_sewer[i]+emissions$pathogen_rur_conforgrid_onsite[i]
        emissions$pathogen_urb_waterforgrid[i]<-sum(c(emissions$to_surface_flushSewer_urb[i],emissions$to_surface_flushSeptic_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_flushPit_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_flushOpen_urb[i],emissions$to_surface_flushUnknown_urb[i],emissions$to_surface_pitSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_pitNoSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinNoSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_compostingToilet_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_bucketLatrine_urb[i],emissions$to_surface_containerBased_urb[i],emissions$to_surface_hangingToilet_urb[i],emissions$to_surface_openDefecation_urb[i],emissions$to_surface_other_urb[i]),na.rm=TRUE)
        emissions$pathogen_rur_waterforgrid[i]<-sum(c(emissions$to_surface_flushSewer_rur[i],emissions$to_surface_flushSeptic_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_flushPit_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_flushOpen_rur[i],emissions$to_surface_flushUnknown_rur[i],emissions$to_surface_pitSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_pitNoSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinNoSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_compostingToilet_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_hangingToilet_rur[i],emissions$to_surface_other_rur[i]),na.rm=TRUE)
      } else{
        emissions$pathogen_urb_conforgrid[i]<-NA
        emissions$pathogen_rur_conforgrid[i]<-NA
        emissions$pathogen_urb_waterforgrid[i]<-(emissions$pathogen_urb_conforgrid_sewer[i]+emissions$pathogen_urb_conforgrid_onsite[i])*HUMAN$removalfraction[i]+sum(c(emissions$to_surface_flushSewer_urb[i],emissions$to_surface_flushSeptic_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_flushPit_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_flushOpen_urb[i],emissions$to_surface_flushUnknown_urb[i],emissions$to_surface_pitSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_pitNoSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_compostingTwinNoSlab_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_compostingToilet_urb[i]*(1-HUMAN$onsiteDumpedland_urb[i]),emissions$to_surface_bucketLatrine_urb[i],emissions$to_surface_containerBased_urb[i],emissions$to_surface_hangingToilet_urb[i],emissions$to_surface_openDefecation_urb[i],emissions$to_surface_other_urb[i]),na.rm=TRUE)
        emissions$pathogen_rur_waterforgrid[i]<-(emissions$pathogen_rur_conforgrid_sewer[i]+emissions$pathogen_rur_conforgrid_onsite[i])*HUMAN$removalfraction[i]+sum(c(emissions$to_surface_flushSewer_rur[i],emissions$to_surface_flushSeptic_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_flushPit_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_flushOpen_rur[i],emissions$to_surface_flushUnknown_rur[i],emissions$to_surface_pitSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_pitNoSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_compostingTwinNoSlab_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_compostingToilet_rur[i]*(1-HUMAN$onsiteDumpedland_rur[i]),emissions$to_surface_hangingToilet_rur[i],emissions$to_surface_other_rur[i]),na.rm=TRUE)
      }
      
      emissions$pathogen_urb_landforgrid[i]<-sum(c(emissions$to_surface_flushSeptic_urb[i]*HUMAN$onsiteDumpedland_urb[i],emissions$to_surface_flushPit_urb[i]*HUMAN$onsiteDumpedland_urb[i],emissions$to_surface_pitSlab_urb[i]*HUMAN$onsiteDumpedland_urb[i],emissions$to_surface_pitNoSlab_urb[i]*HUMAN$onsiteDumpedland_urb[i],emissions$to_surface_compostingTwinSlab_urb[i]*HUMAN$onsiteDumpedland_urb[i],emissions$to_surface_compostingTwinNoSlab_urb[i]*HUMAN$onsiteDumpedland_urb[i],emissions$to_surface_compostingToilet_urb[i]*HUMAN$onsiteDumpedland_urb[i]),na.rm=TRUE)
      emissions$pathogen_rur_landforgrid[i]<-sum(c(emissions$to_surface_flushSeptic_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_flushPit_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_pitSlab_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_pitNoSlab_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_compostingTwinSlab_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_compostingTwinNoSlab_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_compostingToilet_rur[i]*HUMAN$onsiteDumpedland_rur[i],emissions$to_surface_bucketLatrine_rur[i],emissions$to_surface_bucketLatrine_rur[i]),na.rm=TRUE)
    }
    
    emissions$pathogen_urb_fconsewer<-0
    emissions$pathogen_rur_fconsewer<-0
    a<-which(emissions$pathogen_urb_conforgrid>0)
    b<-which(emissions$pathogen_rur_conforgrid>0)
    emissions$pathogen_urb_fconsewer[a]<-emissions$pathogen_urb_con[a]/emissions$pathogen_urb_conforgrid[a]
    emissions$pathogen_rur_fconsewer[b]<-emissions$pathogen_rur_con[b]/emissions$pathogen_rur_conforgrid[b]
    
    emissions$to_surface_flushSewer_urb_out<-emissions$to_surface_flushSewer_urb
    emissions$to_surface_flushSeptic_urb_out<-emissions$to_surface_flushSeptic_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_flushSeptic_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_flushPit_urb_out<-emissions$to_surface_flushPit_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_flushPit_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_flushOpen_urb_out<-emissions$to_surface_flushOpen_urb
    emissions$to_surface_flushUnknown_urb_out<-emissions$to_surface_flushUnknown_urb
    emissions$to_surface_pitSlab_urb_out<-emissions$to_surface_pitSlab_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_pitSlab_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_pitNoSlab_urb_out<-emissions$to_surface_pitNoSlab_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_pitNoSlab_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_compostingTwinSlab_urb_out<-emissions$to_surface_compostingTwinSlab_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_compostingTwinSlab_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_compostingTwinNoSlab_urb_out<-emissions$to_surface_compostingTwinNoSlab_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_compostingTwinNoSlab_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_compostingToilet_urb_out<-emissions$to_surface_compostingToilet_urb*(1-HUMAN$onsiteDumpedland_urb)+emissions$to_surface_compostingToilet_urb*HUMAN$onsiteDumpedland_urb*0.025
    emissions$to_surface_bucketLatrine_urb_out<-emissions$to_surface_bucketLatrine_urb
    emissions$to_surface_containerBased_urb_out<-emissions$to_surface_containerBased_urb
    emissions$to_surface_hangingToilet_urb_out<-emissions$to_surface_hangingToilet_urb
    emissions$to_surface_openDefecation_urb_out<-emissions$to_surface_openDefecation_urb
    emissions$to_surface_other_urb_out<-emissions$to_surface_other_urb
    
    emissions$to_surface_flushSewer_rur_out<-emissions$to_surface_flushSewer_rur
    emissions$to_surface_flushSeptic_rur_out<-emissions$to_surface_flushSeptic_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_flushSeptic_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_flushPit_rur_out<-emissions$to_surface_flushPit_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_flushPit_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_flushOpen_rur_out<-emissions$to_surface_flushOpen_rur
    emissions$to_surface_flushUnknown_rur_out<-emissions$to_surface_flushUnknown_rur
    emissions$to_surface_pitSlab_rur_out<-emissions$to_surface_pitSlab_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_pitSlab_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_pitNoSlab_rur_out<-emissions$to_surface_pitNoSlab_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_pitNoSlab_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_compostingTwinSlab_rur_out<-emissions$to_surface_compostingTwinSlab_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_compostingTwinSlab_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_compostingTwinNoSlab_rur_out<-emissions$to_surface_compostingTwinNoSlab_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_compostingTwinNoSlab_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_compostingToilet_rur_out<-emissions$to_surface_compostingToilet_rur*(1-HUMAN$onsiteDumpedland_rur)+emissions$to_surface_compostingToilet_rur*HUMAN$onsiteDumpedland_rur*0.025
    emissions$to_surface_bucketLatrine_rur_out<-emissions$to_surface_bucketLatrine_rur*0.025
    emissions$to_surface_containerBased_rur_out<-emissions$to_surface_containerBased_rur*0.025
    emissions$to_surface_hangingToilet_rur_out<-emissions$to_surface_hangingToilet_rur
    emissions$to_surface_openDefecation_rur_out<-emissions$to_surface_openDefecation_rur*0.025
    emissions$to_surface_other_rur_out<-emissions$to_surface_other_rur
    
    #WAT NU NOG MIST IS VOOR IEDERE TOILET CATEGORIE URB EN RUR EEN ARRAY EMISSIONS$TO_SEWARAGE_XXX_XXX_OUT
    #EN EMISSIONS$TO_FECALSLUDGE_XXX_XXX_OUT. DAT KAN NA HET TREATMENT STUK GEMAAKT WORDEN
    #DAN MOETEN DE EMISSIONS$TO_SURFACE, EMISSIONS$TO_SEWERAGE EN EMISSIONS$TO_FECAL_SLUDGE OPGETELD
    #WORDEN TOT TOTALE EMISSIONS PER TOILET TYPE (BIJV EMISSIONS$TOTAL_XXX_XXX_OUT) DIE DAN KUNNEN WORDEN
    #UITGESCHREVEN ALS JSON
    
    # 4) return emissions
    return(emissions)
  }
  
}