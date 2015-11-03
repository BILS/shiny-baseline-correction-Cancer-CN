library(shiny)
library(CopyNumber450kCancer)

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
    

 output$autoplot <- renderPlot({
                     if (is.null(input$file1))
                       return(NULL)
                     regions <- input$file1
                     sampleList <- input$file2
                     object <- ReadData(regions$datapath, sampleList$datapath)
                     object <- AutoCorrectPeak(object)
                     plotRegions(object$regions)
            

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
    updateSelectInput(session, "RegionSample", choices = RegionVariables)
    updateSelectInput(session, "RegionChromosome", choices = RegionVariables)
    updateSelectInput(session, "Regionbpstart", choices = RegionVariables)
    updateSelectInput(session, "Regionbpend", choices = RegionVariables)
    updateSelectInput(session, "RegionNumMark", choices = RegionVariables)
    updateSelectInput(session, "RegionMean", choices = RegionVariables)
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
    updateSelectInput(session, "SampleNumber", choices = SampleVariables)
    updateSelectInput(session, "SampleSample", choices = SampleVariables)
    updateSelectInput(session, "SampleComment", choices = SampleVariables)
    SampleInput
  })

   output$sampleButtonG2Raw <- renderUI({
             
       if (is.null(input$file2))
           return(NULL)
       actionButton("SampleActionButton", label = "Data Looks OK NEXT plot Raw")
       })

   output$regionsbuttonsGo2Sample <- renderUI({
             
       if (is.null(input$file1))
           return(NULL)
       actionButton("RegionsActionButtonGo2Sample", label = "Data looks OK Load Sample?")
       })

    output$regionsbuttonsGo2PlotRaw <- renderUI({
             
       if (is.null(input$file1))
           return(NULL)
       actionButton("RegionsActionButtonGo2PlotRaw", label = "Data looks OK plot Raw?")
       })


 
})
