library(shiny)
library(CopyNumber450kCancer)

shinyServer(function(input, output, session) {

# 
observeEvent(input$FileActionButton, {
            updateNavbarPage(session, "baseCN", selected = "Plot auto")
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

  output$csvtable <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    regions <- input$file1
    sampleList <- input$file2
    

    if (is.null(regions))
      return(NULL)
    if (!is.null(sampleList))
	{
	merge(read.csv(regions$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote,nrows=10),read.csv(sampleList$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote,nrows=10))[,-7]
     
        }
     else	 
    
    read.csv(regions$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote,nrows=10)
  })

   output$Actionbutton <- renderUI({
             
       if (is.null(input$file1))
           return(NULL)
       actionButton("FileActionButton", label = "Data Looks OK NEXT")
})
 
})
