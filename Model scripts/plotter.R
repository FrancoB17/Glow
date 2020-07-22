library(rgdal)
library(sp)
library(khroma)

plotter.plot.map <- function(grid,out_file,col, breaks,width,height,boundaries){
  if(missing(out_file)){
    fname <- sprintf("humanemissions_%s_%s.png",SCENARIO$pathogen,SCENARIO$run)
    out_file <- file.path(SCENARIO$model_output,fname)
  }
  if(missing(col)){
    col < -c("slateblue4","slateblue4","blue","skyblue","limegreen", "yellow", "darkorange","red2","red4","red4")
  }
  if(missing(breaks)){
    breaks <- plotter.calc.breaks(grid)
  }
  if(missing(width)){
    width <- 750
  }
  if(missing(height)){
    height <- 750
  }
  tryCatch({
    png(filename = out_file, width = width, height = height, units = "px")
    par(lwd=1,mar=c(6,1,1,1),ps=18,bty="n")
    plot(grid,col=col,breaks=breaks,legend=FALSE,axes=FALSE)
    
    if(!missing(boundaries)){
      plot(boundaries, add=T)
    }
    else{
      log_warn("Missing arguments. Cannot plot boundaries.")
    }
    legend_text <- ""
    # plot legend
    if(SCENARIO$pathogen == "cryptosporidium"){
      legend_text <- "Cryptosporidium emissions (log10 oocysts / year)"
    }
    else if(SCENARIO$pathogen == "rotavirus"){
      legend_text <- "Rotavirus emissions (log10 viral particles / year)" 
    }
    labels <- breaks
    if(breaks[length(breaks)]==Inf){
      labels[length(labels)] <- sprintf(">%s",breaks[length(breaks)-1])
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