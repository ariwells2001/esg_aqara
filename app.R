library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = span(img(src = "aqaralogo.png", height = 45),"AQARA AI")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Loaing data file",tabName="datafile",icon=icon("table")),
      menuItem("Explanatory Data Analysis", tabName = "eda", icon = icon("dashboard")),
      menuItem("Modeling", tabName = "modeling", icon = icon("robot"))
    )
  ),
  dashboardBody(
 
    
    tabItems(
      # First tab content
      tabItem(tabName="datafile",
              fluidRow(
                box(
                  background = "green",
                  fileInput('file1', 'Choose a csv or tsv file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.tsv'
                            )
                  ),
                  checkboxInput('header', 'Header', TRUE)
                ),
                
                box(
                  background = "green", width = 3,
                  radioButtons('sep', 'Separator',
                               c(Comma=',',
                                 Semicolon=';',
                                 Tab='\t'),
                               ',')
                ),
                box (
                  background = "green", width = 3,
                  radioButtons('quote', 'Quote',
                               c(None='',
                                 'Double Quote'='"',
                                 'Single Quote'="'"),
                               '"')
                )
              ),
              
              fluidRow(
                
                box(
                  title = "Uploaded Data Table", background = "orange", width = 12,
                 numericInput("nrs", "No. of Rows:", 10),
                 style = 'font-size:10px;color:blue;',
                  tableOutput("fuploadtable")
                ),
                
                box(
                  title = "Uploaded Data Histogram", background = "yellow", width = 6,      
                  uiOutput("columns"),
                  plotOutput("uploadplot")
                ),
                
                #####additional features
                box(
                  title = "Summary Plot", background = "yellow", width = 6,
                  plotOutput("uploadsummaryplot")
                )
              )
              ),
      
      tabItem(tabName = "eda",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "modeling",
              h2("Modeling Here")
      )
    )
  )
)

server <- function(input, output) {
  sensorDF <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return (NULL)
    
    read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
  })
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$fuploadtable <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
       return (NULL)
    
  head(sensorDF(), n = input$nrs)
 
 })
  
  
  
  
  
  
  output$columns = renderUI({

    uploaddf1 <- sensorDF()
    
    selectInput('columns2', 'Columns', names(uploaddf1))
  })
  
  

  output$uploadplot <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return (NULL)
    
    
    up_var <- sensorDF()[,c(input$columns2)]
    hist(up_var,main= "Histogram for Uploaded Data", xlab=input$columns2,col="blue")
   
    
  })
  
  output$uploadsummaryplot <- renderPlot({
  
    inFile <- input$file1

    if (is.null(inFile))
      return (NULL)

    pairs(sensorDF(),main="Summary Plot",pch=21,bg=c("red","green","blue"))
    
  })
  
}

shinyApp(ui, server)
