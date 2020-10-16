library(rgdal)
library(sp)
library(khroma)

#' Plot pathogen map
#'
#' @param grid 
#' @param out_file 
#' @param col 
#' @param breaks 
#' @param width 
#' @param height 
#' @param boundaries 
#'
#' @return
#' @export
#'
#' @examples
plotter.plot.map <- function(grid,out_file,col, breaks,width,height,boundaries,extent=NULL){
  # determine a plot title. Either pathogen name, pathogen type or nothing
  pathogen_text <- if(!is.null(PATHOGEN$name) || !PATHOGEN$name == '') PATHOGEN$name else if(!is.null(PATHOGEN$pathogenType || !PATHOGEN$pathogenType))SCENARIO$pathogenType else ""
  # construct output path for plot if out_file is not givevn.
  if(missing(out_file)){
    fname <- sprintf("humanemissions_%s_%s.png",pathogen_text,SCENARIO$run)
    out_file <- file.path(SCENARIO$model_output,fname)
  }
  # set default colors if array of colors is missing
  if(missing(col)){
    col < -c("slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")
  }
  # calculate class breaks for plot based on grid if the breaks are not given to function.
  if(missing(breaks)){
    breaks <- plotter.calc.breaks(grid)
  }
  # set default width and height if not specified.
  if(missing(width)){
    width <- 750
  }
  if(missing(height)){
    height <- 750
  }
  tryCatch({
    png(filename = out_file, width = width, height = height, units = "px")
    # we set bg to gray, because it the color of missing data
    par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n",bg="gray")
    plot(grid,col=col,breaks=breaks,legend=FALSE,axes=FALSE,ext=extent)
    # plot administrative borders if given to function on top of the grid.
    if(!missing(boundaries)){
      plot(boundaries, add=T)
    }else{
      log_warn("Missing arguments. Cannot plot boundaries.")
    }
    unit <- ""
    # plot legend
    if(PATHOGEN$name == "cryptosporidium" || PATHOGEN$pathogenType == "Protozoa"){
      unit <- "(log10 oocysts / year)"
    }else if(PATHOGEN$name == "rotavirus" || PATHOGEN$pathogenType == "Virus"){
      unit <- "(log10 viral particles / year)"
    }
    
    legend_text <- sprintf("%s emissions %s",pathogen_text,unit)
    labels <- breaks
    if(breaks[1] < 0){
      labels[1] <- "NA"
    }
    # if last class break is equal to infinity set label > previous last class break.
    if(breaks[length(breaks)]==Inf){
      labels[length(labels)] <- sprintf(">%s",breaks[length(breaks)-1])
      # modify the last class break value in case of infinity for the legend by using the previous last class break and difference between breaks.
      breaks[length(breaks)] <- breaks[length(breaks)-1] + (breaks[length(breaks)-1] - breaks[length(breaks)-2])
    }
    plot(grid, legend.only=TRUE, col=col,breaks=breaks,horizontal=TRUE,
         legend.width=1, legend.shrink=0.75,
         axis.args=list(at=breaks,
                        labels=labels,
                        cex.axis=1,font=1.4),
         legend.args=list(text=legend_text, font=1.8, cex=1.2))
  },finally={
    dev.off()
  })
  return(out_file)
}

plotter.plot.map.level4 <- function(emissions,out_file,col,breaks,width,height,boundaries,extent=NULL){
  
  library(grid)

  if(!missing(boundaries)){
    #prepare data and make it ready for plotting. That includes linking it to the boundaries data
    
    boundariesdf<-data.frame(boundaries@data$GID_4,boundaries@data$NAME_4)
    colnames(boundariesdf)<-c("gid","name")
    boundariesdf[]<-lapply(boundariesdf,as.character)
    boundariesdf$iso<-NA
    
    for(i in 1:length(HUMAN_DATA$gid)){
      a<-which(boundariesdf$gid==HUMAN_DATA$gid[i])
      if(length(a)>0){
        boundariesdf$iso[a]<-HUMAN_DATA$iso[i]
      }
    }
    
    boundaries@data$logemissions<-NA    
    for(j in 1:length(emissions$iso)){
      a<-which(boundariesdf$iso==emissions$iso[j])
      boundaries@data$logemissions[a]<-emissions$total_log[j]
    }
    
    # determine a plot title. Either pathogen name, pathogen type or nothing
    pathogen_text <- if(!is.null(PATHOGEN$name) || !PATHOGEN$name == '') PATHOGEN$name else if(!is.null(PATHOGEN$pathogenType || !PATHOGEN$pathogenType))SCENARIO$pathogenType else ""
    # construct output path for plot if out_file is not givevn.
    if(missing(out_file)){
      fname <- sprintf("humanemissions_%s_%s.png",pathogen_text,SCENARIO$run)
      out_file <- file.path(SCENARIO$model_output,fname)
    }
    # set default colors if array of colors is missing
    if(missing(col)){
      col < -c("slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")
    }
    # calculate class breaks for plot based on grid if the breaks are not given to function.
    if(missing(breaks)){
      breaks <- plotter.calc.breaks(grid)
    }
    # set default width and height if not specified.
    if(missing(width)){
      width <- 750
    }
    if(missing(height)){
      height <- 750
    }
    tryCatch({
      png(filename = out_file, width = width, height = height, units = "px")
      # we set bg to gray, because it the color of missing data
      par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n",bg="gray")
      print(spplot(boundaries,"logemissions",col.regions=col,at=breaks,colorkey=list(labels = list(width = 4, cex = 2))))
      
      unit <- ""
      # plot legend
      if(PATHOGEN$name == "cryptosporidium" || PATHOGEN$pathogenType == "Protozoa"){
        unit <- "(log10 oocysts / year)"
      } else if(PATHOGEN$name == "rotavirus" || PATHOGEN$pathogenType == "Virus"){
        unit <- "(log10 viral particles / year)"
      }
      
      legend_text <- sprintf("%s emissions %s",pathogen_text,unit)      
      
      grid.text("legend_text", x=unit(0.85, "npc"), y=unit(0.55, "npc"), rot=90)
      grid.text("missing data",x=unit(0.88,"npc"),y=unit(0.05,"npc"))
      
##    plot(,col=col,breaks=breaks,legend=FALSE,axes=FALSE,ext=extent)
      # plot administrative borders if given to function on top of the grid.
 #     plot(boundaries, add=T)

    },finally={
      dev.off()
    })
  }
  else{
    log_warn("Missing arguments. Cannot plot.png file.")
  }
  

  return(out_file)
}

plotter.get.colors.khroma <- function(color_scheme_name,nbreaks){
  discrete_rainbow <- colour(color_scheme_name)
  colors<-c("grey","white",discrete_rainbow(nbreaks)) 
  return(colors)
}

plotter.calc.breaks <- function(grid){
  min_value <- minValue(grid)
  max_value <- maxValue(grid)
  brks<-c(floor(min),floor(min)+(ceiling(max)-floor(min))/10,floor(min)+2*(ceiling(max)-floor(min))/10,floor(min)+3*(ceiling(max)-floor(min))/10,floor(min)+4*(ceiling(max)-floor(min))/10,floor(min)+5*(ceiling(max)-floor(min))/10,floor(min)+6*(ceiling(max)-floor(min))/10,floor(min)+7*(ceiling(max)-floor(min))/10,floor(min)+8*(ceiling(max)-floor(min))/10,floor(min)+9*(ceiling(max)-floor(min))/10,ceiling(max))
  return(brks)
}