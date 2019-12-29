#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel("keyword Sentences Match "),
    
    sidebarLayout( 
        
        sidebarPanel(  
            
            fileInput("file1", "Upload data (csv file with header)",  accept = c("text/csv",
                                                                                 "text/comma-separated-values,text/plain",
                                                                                 ".csv")),
            
            textInput("keywd", "Enter Keywords to search", value = "", width = NULL,
                      placeholder = NULL) ),   # end of sidebar panel
        
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Overview",
                                 h4(p("Data input")),
                                 p("This app supports only comma separated values (.csv) data file. CSV data file should have headers and the first column of the file should have row names.",align="justify"),
                                 p("Please refer to the link below for sample csv file."),
                                 a(href="https://github.com/sudhir-voleti/sample-data-sets/blob/master/Segmentation%20Discriminant%20and%20targeting%20data/ConneCtorPDASegmentation.csv"
                                   ,"Sample data input file"),   
                                 br(),
                                 h4('How to use this App'),
                                 p('To use this app, click on', 
                                   span(strong("Upload data (csv file with header)")),
                                   'and uppload the csv data file. You can also change the number of clusters to fit in k-means clustering')),
                        
                        tabPanel("Data Corpus", 
                               tableOutput("contents")),
                        
                        tabPanel("Data -keyword-filtered Corpus", 
                                 tableOutput("kw")),
                        
                        tabPanel("Sentence Tokenization", 
                                 tableOutput("sent")),
                        
                        tabPanel("Bar Chart",
                                 plotOutput('bar')),
                        
                        tabPanel("Word Cloud",
                                 plotOutput('wc'))
            ) # end of tabsetPanel
        )# end of main panel
    ) # end of sidebarLayout
)  # end if fluidPage
) # end of UI
