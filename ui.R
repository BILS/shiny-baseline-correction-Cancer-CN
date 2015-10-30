library(shiny)
library(markdown)

shinyUI(navbarPage("Baseline Correction for Copy Number Data from Cancer Samples", id="baseCN",
  tabPanel("Description",
          fluidPage(
   	        titlePanel("Description"),
                fluidRow(
                        column(5,includeMarkdown("description.md")) 
                        )  
                    )
           
         ),
  tabPanel("Upload",
            fluidPage(
                titlePanel("Upload data"),
                   sidebarLayout(

                       # Sidebar with a slider input
                       sidebarPanel(
                            #deg
                            fileInput('file1', 'Choose Regions CSV File', accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                            fileInput('file2', '*Optional* Choose samplenamne  CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                            tags$hr(),
                            tags$div(class="header", checked=NA,
                            tags$p("CSV manipulation")
                                   ),

                            checkboxInput('header', 'Header', TRUE),radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                            radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"')
                                  ),

                             # Tableoutput of files in mainpanel
                             mainPanel(
                                tableOutput('csvtable'),
                                uiOutput('Actionbutton')
                                )
                  )
             )
          
                   
          ),
  tabPanel("Plot auto",
          fluidPage(
                titlePanel("Autocorrected plot"),
                         sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("autoplot")
    )
  )
                      
                   )
          
         
          ),
  tabPanel("Table",
            fluidPage(
                titlePanel("Table"),
                HTML("Table")
          
                    )
          )
))


