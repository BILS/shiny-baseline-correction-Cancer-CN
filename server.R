library(shiny)

shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    regions <- input$file1
    sampleList <- input$file2
    

    if (is.null(regions))
      return(NULL)
    
    read.csv(regions$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote,nrows=10)
  })
})
