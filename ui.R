library(shiny)
library(markdown)

shinyUI(navbarPage("Baseline Correction for Copy Number Data from Cancer Samples", id="baseCN",
  tabPanel("Description",
   fluidPage(
   titlePanel("Description"),
   fluidRow(
     column(5,
     includeMarkdown("description.md")
         ) 
   )  )

  ), 
  
  tabPanel("Uploading Files",
  fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Regions CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
       fileInput('file2', '*Optional* Choose samplenamne  CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
      tags$hr(),
      	HTML('<b>CSV '),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
      
    ),

    mainPanel(
      HTML('<b>CSV file manipulate'),
      tableOutput('contents'),
      uiOutput('Actionbutton')
      
    )
  )
)),
  tabPanel("Component 2"),
  tabPanel("Component 3")




))
