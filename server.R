library(shiny)
library(ggplot2) 

#New CopyNumber Functions
#region_file fileinput path from Shiny
#regions_colnames colnames vector from shiny input
#sample_list file input path from Shiny input
#sample_colnames colnames vector from shiny inputs
ReadData<-function(session,regions_file, regions_colnames, sample_list,sample_colnames){

    regions<-read.csv(regions_file,stringsAsFactors =FALSE)

    regions<-regions[,regions_colnames]
    colnames(regions)<-c("Sample","Chromosome","bp.Start","bp.End","Num.of.Markers","Mean")
    max_plots<<-length(unique(regions$Sample))
    updateSliderInput(session, "NumberSampleSlider", max=max_plots )
    if(!missing(sample_list)){
        SL<-read.csv(sample_list,stringsAsFactors =FALSE)
        SL<-SL[,sample_colnames]
        colnames(SL)<-c("Number","Sample","Comment")
        }
    
    object <- list(
        regions = regions,
        regions_save = regions,
        regions_auto = regions
        )
    class(object) <- "CopyNumber450kCancer_data"
    if (missing(sample_list)) {
        Number <- c(1:length(unique(regions$Sample)))
        Sample <- unique(regions$Sample)
        Comment <- c(rep(" ",length(unique(regions$Sample))))
        SL <- data.frame(Number, Sample, Comment, stringsAsFactors = F)
    }
    
    mod<-as.data.frame(SL$Sample,stringsAsFactors =FALSE)  # copy to store the modification in it
    mod[,2:6]<-0
    mod[,6]<-"No"
    mod[is.na(mod)] <- 0
    colnames(mod)<-c("Sample","Lower_selected_level","Upper_selected_level","Mean_of_selected_regions","Shifting","Reviewed?")
    object$mod <- mod
    object$SL <- SL
      
    mod_auto<-mod[,c("Sample","Mean_of_selected_regions","Shifting","Reviewed?")]
    colnames(mod_auto)<-c("Sample","Auto_Maximum_Peak","Shifting","Auto_Corrected?")
    object$mod_auto <- mod_auto
  
    object
}

#this uses the regions file: Chromosome should be in this format: "chr1"
#similar to the original function, this one use only the cutoff
plotRegions<-function(object, chr, start, end, cutoff=0.1,markers=20, ...) {   
  sample_segments <- object
  
  #if(hasArg(markers)){
  #  sample_segments$Mean[which(sample_segments$Num.of.Markers<=markers)]<-0
  #}
  
  segment_values <- as.numeric(sample_segments[,"Mean"])
  segment_colors <- rep("black", nrow(sample_segments))
  
  if (missing(cutoff)) {
    cutoff<-(0.1)
  }
  
  segment_colors[as.numeric(segment_values) >= cutoff] <- "green"
  segment_colors[as.numeric(segment_values) <= -cutoff] <- "red"
  
  if (missing(chr)) {
    # Plotting the whole genome
    chromosomes <- unique(sample_segments[, "Chromosome"])
    site_per_chr <- cumsum(c(0, sapply(chromosomes, function(chr) max(as.numeric(sample_segments[sample_segments[,"Chromosome"] == chr, "bp.End"])))))
    offset <- site_per_chr - min(as.numeric(sample_segments[sample_segments[, "Chromosome"] == "chr1", "bp.Start"])) # 1 instead of "chr1" #as.numeric(gsub("\\D", "", x))
    start <- 0
    end <- as.numeric(max(site_per_chr))
    x_axis_type <- "n"
  } else {
    # Plotting a region
    if (missing(start)) {
      start <- 0
    }
    
    if (missing(end)) {
      end <- as.numeric(max(sample_segments[sample_segments[, "Chromosome"] == chr, "bp.End"]))
    }
    
    chromosomes <- chr
    offset <- 0
    x_axis_type <- NULL
  }
  
  yMin <- (-1) #min(c(-1, as.numeric(sample_segments[significant_segments, "Mean"])))
  yMax <- 1 #max(c(1, as.numeric(sample_segments[significant_segments, "Mean"])))
  
  #if (missing(ylab)) {
  # ylab<-""
  #}
  
  myPlot <- plot(range(start, end), range(yMin, yMax), type = "n",axes=FALSE, xaxt = x_axis_type, xlab="",ylab="", ...) #ylab="Mean",
  
  #---this function to plot the tick on the right side
  tick.tick<-function (nx = 2, ny = 2, tick.ratio = 0.5) {
    ax <- function(w, n, tick.ratio) {
      range <- par("usr")[if (w == "x") 
        1:2
        else 3:4]
      tick.pos <- if (w == "x") 
        par("xaxp")
      else par("yaxp")
      distance.between.minor <- (tick.pos[2] - tick.pos[1])/tick.pos[3]/n
      possible.minors <- tick.pos[1] - (0:100) * distance.between.minor
      low.minor <- min(possible.minors[possible.minors >= range[1]])
      if (is.na(low.minor)) 
        low.minor <- tick.pos[1]
      possible.minors <- tick.pos[2] + (0:100) * distance.between.minor
      hi.minor <- max(possible.minors[possible.minors <= range[2]])
      if (is.na(hi.minor)) 
        hi.minor <- tick.pos[2]
      axis(if (w == "x") 
        1
        else 4, seq(low.minor, hi.minor, by = distance.between.minor), 
        labels = FALSE, tcl = par("tcl") * tick.ratio)
    }
    if (nx > 1) 
      ax("x", nx, tick.ratio = tick.ratio)
    if (ny > 1) 
      ax("y", ny, tick.ratio = tick.ratio)
    invisible()
  }
  
  if (missing(chr)) {
    xlabs <- sapply(2:length(site_per_chr), function(j) {
      ((site_per_chr[j] - site_per_chr[j - 1])/2) + site_per_chr[j - 1]
    })
    
    axis(1, at = xlabs, labels = chromosomes, lty = 0, las = 2, ...)
    axis(4)
    tick.tick(nx=0,ny=2, tick.ratio=1.6)
    tick.tick(nx=0,ny=10, tick.ratio=0.6)
    mtext("L-value", side = 4, line = 2, cex = par("cex.lab"))
    box()
    abline(v = site_per_chr, lty = 3)
    abline(h = c(0,-cutoff,cutoff), lty = 3)
  }
  
  lapply(1:length(chromosomes), function(i) {
    used_segments <- sample_segments[, "Chromosome"] == chromosomes[i]
    colors <- segment_colors[used_segments]
    starts <- as.numeric(sample_segments[used_segments, "bp.Start"]) + offset[i]
    ends <- as.numeric(sample_segments[used_segments, "bp.End"]) + offset[i]
    y <- as.numeric(sample_segments[used_segments, "Mean"])
    graphics::segments(starts, y, ends, y, col = colors, lwd = 2, lty = 1)
  })
  
  myPlot
}


PlotRawData<-function(object, select=1, plots=TRUE,cutoff=0.1,markers=20, comments =FALSE,...){
  
    name <- object$SL[select,"Sample"] # get the sample name
    sam <- object$regions[which(object$regions$Sample %in% as.character(name)),]   #get the sample segments
    if(hasArg(markers)){ sam<-sam[which(sam$Num.of.Markers>markers),] }

    #to prepare the spaces for the plots
    par(mfrow=c(1,2),mar=c(0,0,2,0),oma=c(0,0,0,4))
    layout(matrix(c(1,2),1,2,byrow=TRUE), widths=c(3,21), heights=c(10), TRUE) 
  
    #calculate the density
    forDen<-sam[which(sam$Chromosome!="chrX" & sam$Chromosome!="chrY"),c("Num.of.Markers","Mean")]
    d<-density(forDen$Mean,weights=forDen$Num.of.Markers/sum(forDen$Num.of.Markers),na.rm=TRUE,kernel = c("gaussian"),adjust=0.15,n=512)
    #plot the density
    plot(d$y,d$x,ylim=c(-1,1),type='l',ylab="",xlab="",axes=FALSE,xlim=rev(range(d$y)))
    abline(h = c(0,-cutoff,cutoff), lty = 3)
    box()
    legend("bottomleft", legend="Density",cex=1)
  
    #plot the regions
    plotRegions(sam,cutoff=cutoff,markers=markers,main=c(paste("Sample ",select,":",object$SL[which(object$SL$Sample %in% as.character(name)),"Sample"]), ...))
    if (comments){
        legend("topleft", legend=paste("Comment:",object$SL[which(object$SL$Sample %in% as.character(name)),"Comment"]),cex=0.75)
        }
}




shinyServer(function(input, output, session) {

#Do something on event liks push buttons
observeEvent(input$RegionsActionButtonGo2Sample, {
            updateNavbarPage(session, "baseCN", selected = "Upload sample list")
        } )
 
observeEvent(input$SampleActionButton, {
            updateNavbarPage(session, "baseCN", selected = "Plot raw")
        } )

observeEvent(input$RegionsActionButtonGo2PlotRaw, {
            updateNavbarPage(session, "baseCN", selected = "Plot raw")
        } )
    

 output$plotraw <- renderUI({
                     if (is.null(input$file1))
                       return(NULL)
                     
                     regions <- input$file1
                     region_colnames <- c(input$RegionSample,input$RegionChromosome,input$Regionbpstart,input$Regionbpend,input$RegionNumMark,input$RegionMean)
                     if(!is.null(input$file2)){
                         sample_list <- input$file2
		         sample_list_colnames <- c(input$SampleNumber, input$SampleSample, input$SampleComment)
                         object<<-ReadData(session,regions$datapath,region_colnames, sample_list$datapath, sample_list_colnames)
                     }
                     else{object<<-ReadData(session,regions$datapath,region_colnames)}
                     for (i in 1:max_plots) {
                          # Need local so that each item gets its own number. Without it, the value
                          # of i in the renderPlot() will be the same across all instances, because
                          # of when the expression is evaluated.
                          local({
                              my_i <- i
                              plotname <- paste("Sample", my_i, sep="")
                              plotcheckbox <- paste("plotcheckbox", my_i, sep="")

                              output[[plotname]] <- renderPlot({
                              PlotRawData(object, select=my_i, plots=TRUE,cutoff=input$NumberCutoffSlider,markers=input$NumberMarkerSlider, comments=input$ShowComments)
                              })
                              output[[plotcheckbox]] <- renderUI({checkboxInput(paste("PlotRawSamplecheckbox", my_i, sep=""),paste("Select Sample", my_i, sep="") , FALSE)
                              })
                          })
                      }
 
                     plot_output_list <- lapply(1:input$NumberSampleSlider, function(i) {
                         plotname <- paste("Sample", i, sep="")
                         plotcheckbox <- paste("plotcheckbox", i, sep="")
                         tags$div(class = "group-output",
                             uiOutput(plotcheckbox),
                             plotOutput(plotname)
                          )
                     

                     })
  # Convert the list to a tagList - this is necessary for the list of items
   # to display properly.
   do.call(tagList, plot_output_list)

})


  output$csvtableRegions <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    regions <- input$file1
    if (is.null(regions))
      return(NULL)
    
    RegionInput=read.csv(regions$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote,nrows=10)
    RegionVariables=names(RegionInput)
    updateSelectInput(session, "RegionSample", choices = RegionVariables, selected=grep("sample|name|sample([:blank:]|[:punct:])name|code|id",RegionVariables , value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "RegionChromosome", choices = RegionVariables, selected=grep("chromosome|chr|chromo",RegionVariables , value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "Regionbpstart", choices = RegionVariables, selected=grep("bp([:blank:]|[:punct:])start|start|chromStart|from,",RegionVariables , value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "Regionbpend", choices = RegionVariables, selected=grep("bp([:blank:]|[:punct:])End|ends|end|chromoEnd|to",RegionVariables , value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "RegionNumMark", choices = RegionVariables, selected=grep("Num([:blank:]|[:punct:])of([:blank:]|[:punct:])Markers|markers|probes| number([:blank:]|[:punct:])of([:blank:]|[:punct:])probes|number([:blank:]|[:punct:])of([:blank:]|[:punct:])markers",RegionVariables , value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "RegionMean", choices = RegionVariables, selected=grep("Mean|log|value|meanlog|L-value",RegionVariables , value=TRUE,ignore.case =TRUE))
    
    RegionInput
  
  })
 output$csvtableSample <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    sample <- input$file2
    if (is.null(sample))
      return(NULL)
    
    SampleInput=read.csv(sample$datapath, header=input$headersamp, sep=input$sepsamp, 
				 quote=input$quotesamp,nrows=10)
    SampleVariables=names(SampleInput)
    updateSelectInput(session, "SampleNumber", choices = SampleVariables, selected=grep("number|sample([:blank:]|[:punct:])number", SampleVariables, value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "SampleSample", choices = SampleVariables, selected=grep("sample|name|sample([:blank:]|[:punct:])name|code|id",SampleVariables , value=TRUE,ignore.case =TRUE))
    updateSelectInput(session, "SampleComment", choices = SampleVariables, selected=grep("comment", SampleVariables, value=TRUE,ignore.case =TRUE))
    SampleInput
  })

   output$sampleButtonG2Raw <- renderUI({
             
       if (is.null(input$file2))
           return(NULL)
       actionButton("SampleActionButton", label = "Data looks OK. --> Plot samples?")
       })

   output$regionsbuttonsGo2Sample <- renderUI({
             
       if (is.null(input$file1))
           return(NULL)
       actionButton("RegionsActionButtonGo2Sample", label = "Data looks OK. --> Load sample sheet?")
       })

    output$regionsbuttonsGo2PlotRaw <- renderUI({
             
       if (is.null(input$file1))
           return(NULL)
       actionButton("RegionsActionButtonGo2PlotRaw", label = "Data looks OK. --> Plot samples?")
       })

output$LoadDataButtons<- renderUI({
tagList(
tags$p(actionButton("LoadSampleData", label = "Load Sample Data")),
tags$p(actionButton("UpLoadData", label = "Upload Data")),
actionButton("LoadFromCancerAtlasData", label = "Browse and load Data from Cancer Atlas")
)
})

 
})
