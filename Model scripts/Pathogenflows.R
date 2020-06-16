#devtools::install_github('mverbyla/pathogenflows')
library(pathogenflows)

pathogenflow.model.run <- function(input_data,pathogen){
  emissions <- data.frame(iso=input_data$iso,subarea=input_data$subarea)
  # 1) extract pathogen type from pathogen
  pathogenType <- "Virus"
  area_types <- c("urban","rural")
  human_age_types <- c("child","adult")
  all_loadings <- list()
  # 2) call loadings function 4 times for urban-adult/child prevalence and rural-adult/child prevalence
  for(area_type in area_types){
    for(human_age_type in human_age_types ){
      #2a) prepare data for urban and rural
      onsite_data <- pathogenflow.model.get.input(input_data,area_type,human_age_type)
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
        emissions_col_name_postfix <- sprintf("%s_%s",sanitation_type,area_type)
        emissions_col_to_surface <- sprintf("%s_%s","to_surface",emissions_col_name_postfix)
        emissions_col_sewerage <- sprintf("%s_%s","sewerage",emissions_col_name_postfix)
        to_surface <- emissions_sanitation$toSurface[j]
        to_sewerage <- emissions_sanitation$sewerage[j]
        # store only to_surface and sewerage
        emissions[[emissions_col_to_surface]][i] <- to_surface
        emissions[[emissions_col_sewerage]][i] <- to_sewerage
      }
    }
  }
  
  # 3) create similair emissions data.frame as original human emissions
  
  # 4) return emissions
  return(emissions)
}

pathogenflow.model.get.input <- function(input,area_type,human_type){
  #@Nynke: population is population per human type (adult/child) or total population?
  col_names <- c("iso","scenario","region","population","sheddingRate","prevalence","flushSewer","flushSeptic","flushPit","flushOpen",
                          "flushUnknown","pitSlab","pitNoSlab","compostingTwinSlab","compostingTwinNoSlab","compostingToilet","bucketLatrine",
                          "containerBased","hangingToilet","openDefecation","other","isShared","sewerLeak","emptied","isWatertight","hasLeach","coverBury",	
                          "emptiedTreatment","emptyFrequency","pitAdditive","flushElsewhere","pitVIP","pitTraditional","otherLatrine","otherImproved",
                          "otherUnimproved","dontKnow","pitLined", "pitUnlined") 
  onsite_data <- data.frame(matrix(ncol=length(col_names),nrow = dim(input)[1]))
  colnames(onsite_data) <- col_names
  onsite_data$iso <- input$iso
  onsite_data$region <- input$subarea
  onsite_data$sheddingRate <- input$sheddingRate

  input_col_names <- colnames(input)
  if(area_type=="urban"){
    postfix <- "urb"
    onsite_data$population <- input$population*input$fraction_urban_pop
  }
  else if(area_type=="rural"){
    postfix <- "rur"
    onsite_data$population <- input$population* (1- input$fraction_urban_pop)
  }
  # filter for urban and rural
  filtered_cols <- input_col_names[str_ends(input_col_names,postfix)]
  for(input_colname in filtered_cols){
    # get corresponding colname for onsite_data
    onsite_colname <- str_replace(input_colname,sprintf("_%s",postfix),"")
    # check if it part of onsite_data
    if(onsite_colname %in% col_names){
      onsite_data[[onsite_colname]] <- input[[input_colname]]
    }
  }
  incidence = 0
  if(human_type == "child"){
    onsite_data$population <- input$population * input$fraction_pop_under5
    if(area_type == "urban"){
      incidence <- input$incidence_urban_under5
    }
    else if(area_type == "rural"){
      incidence <- input$incidence_rural_under5 
    }
  }
  else if(human_type == "adult"){
    onsite_data$population <- input$population * (1-input$fraction_pop_under5)
    if(area_type == "urban"){
      incidence <- input$incidence_urban_5plus
    }
    else if(area_type == "rural"){
      incidence <- input$incidence_rural_5plus 
    }
  }
  # @Nynke: Is this ok?
  onsite_data$prevalence <- incidence*input$sheddingRate*input$shedding_duration
  return(onsite_data)
}