#devtools::install_github('mverbyla/pathogenflows')
library(pathogenflows)

pathogenflow.model.run <- function(pathogen){
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
  }
  
  # 3) create similair emissions data.frame as original human emissions
  
  #In_Fecal_Sludge is the waste from onsite systems going to the WWTP
  #In_Sewage is the waste from sewerage going to the WWTP
  #to_Surface is the waste that goes either to the land or to the surface water directly.
  #
  #from to_Surface the following goes to water directly: flushSewer, flushSeptic*(1-onsiteDumpedland), 
  #flushPit*(1-onsiteDumpedland), flushOpen, flushUnknown, pitSlab*(1-onsiteDumpedland), pitNoslab*(1-onsiteDumpedland)
  #compostingTwinSlab*onsiteDumpedwater, compostingTwinNoSlab*onsiteDumpedwater, compostingToilet*onsiteDumpedwater
  #bucketLatrine urban, containerBased urban, hangingToilet, openDefecation urban
  #
  #from to_Surface the following goes to land: flushSeptic*onsiteDumpedland, flushPit*onsiteDumpedland, 
  #pitSlab*onsiteDumpedland, compostingTwinSlab*onsiteDumpedland, compostingTwinNoSlab*onsiteDumpedland,
  #compostingToilet*onsiteDumpedland, bucketLatrine rural, containerBased rural, openDefecation rural
  #
  #???Ik weet niet zeker, maar klopt het dat emissions de file is die alle output heeft?
  #???Ik weet niet hoe je de urbane en rurale waarden apart hebt bedacht...
  #???Ik weet dus niet zo goed hoe ik onderstaande punten mbv bovenstaande aannames (die nog even
  #bij Matt liggen ook voor definitieve OK kan opzetten...)
  
  # emissions$pathogen_urb_conforgrid_sewer<- 
  # emissions$pathogen_rur_conforgrid_sewer<-
  # emissions$pathogen_urb_conforgrid_onsite<-
  # emissions$pathogen_rur_conforgrid_onsite<-
  # emissions$pathogen_urb_waterforgrid<-
  # emissions$pathogen_rur_waterforgrid<-
  # emissions$pathogen_urb_landforgrid<-
  # emissions$pathogen_rur_landforgrid<-
  
  # 4) return emissions
  return(emissions)
}

pathogenflow.model.get.input <- function(area_type,human_type){
  col_names <- c("iso","scenario","region","population","sheddingRate","prevalence","flushSewer","flushSeptic","flushPit","flushOpen",
                          "flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine",
                          "containerBased","hangingToilet","openDefecation","other","isShared","sewerLeak","emptied","isWatertight","hasLeach","coverBury",	
                          "emptiedTreatment","emptyFrequency","pitAdditive","flushElsewhere","pitVIP","pitTraditional","otherLatrine","otherImproved",
                          "otherUnimproved","dontKnow","pitLined", "pitUnlined") 
  onsite_data <- data.frame(matrix(ncol=length(col_names),nrow = dim(HUMAN_DATA[1]))
  colnames(onsite_data) <- col_names
  onsite_data$iso <- HUMAN_DATA$iso
  onsite_data$region <- HUMAN_DATA$subarea
  onsite_data$sheddingRate <- HUMAN_DATA$sheddingRate

  input_col_names <- colnames(HUMAN_DATA
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
  incidence = 0
  if(human_type == "child"){
    onsite_data$population <- HUMAN_DATA$population * HUMAN_DATA$fraction_pop_under5
    if(area_type == "urban"){
      incidence <- HUMAN_DATA$incidence_urban_under5
    }
    else if(area_type == "rural"){
      incidence <- HUMAN_DATA$incidence_rural_under5 
    }
  }
  else if(human_type == "adult"){
    onsite_data$population <- HUMAN_DATA$population * (1-HUMAN_DATA$fraction_pop_under5)
    if(area_type == "urban"){
      incidence <- HUMAN_DATA$incidence_urban_5plus
    }
    else if(area_type == "rural"){
      incidence <- HUMAN_DATA$incidence_rural_5plus 
    }
  }
  # @Nynke: Is this ok?
  onsite_data$prevalence <- incidence*HUMAN_DATA$sheddingRate*HUMAN_DATA$shedding_duration
  return(onsite_data)
}