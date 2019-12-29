#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tokenizers)
library(dplyr)
library(janeaustenr)
library(wordcloud)
library(tm)

suppressPackageStartupMessages({
    if (!require(tidyverse)) {install.packages("tidyverse")}
    if (!require(tidytext)) {install.packages("tidytext")}
    
    library(tidyverse)
    library(tidytext)
    library(ggplot2)
})
#ss <<- ""
df_sent=""


searchWord <- function(zz,word) {
    #i=0
    #print("============Inside function Searchword========")
    
    require(tibble)
    textdf = tibble(text = zz)
    sent_tokenized = textdf %>% unnest_tokens(sentence, text, token = "sentences")
    df_sent<-data.frame(sent_tokenized)
    #u<-data.frame(sent_tokenized)
    #print(df_sent)
    size<-nrow(df_sent)
    #paste0("Size:",size)
    i=1
    foundFlag=0
    str=""
    while (i <= size ){
        v<-df_sent[i,]
     
        
        if (grepl(word,v) == TRUE)
        {
            print("Found word:")
            print(word)
            print("Searched sentences:")
            #endl<-paste(v,sep= "\n")
            str<-paste0(str,v)
            strsplit(str,"/n")[[1]]
            foundFlag=1
        }
        else 
        {
            if( i >= size && foundFlag == 0){
                print("Not Found word:")
                print(word)
                str="Not Found"
                #print(i)
                #print(size)
            }
        }
        i = i+1
    }
    print("%%%%%%%%%%%%%Concat String%%%%%%%%%%%%")
    print(str)
    return(str)
}




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output)  {

    output$bar <- renderPlot({
        
        df <- read.csv(input$file1$datapath,
                       header = FALSE,
                       sep = ",",
                       stringsAsFactors = FALSE
        )
        
        ll<-df$V3
        
        tempcorpus = lapply(ll, tolower)
        tempcorpus<-Corpus(VectorSource(tempcorpus))
        tempcorpus<-tm_map(tempcorpus,removePunctuation)
        tempcorpus<-tm_map(tempcorpus,stripWhitespace)
        tempcorpus<-tm_map(tempcorpus,removeNumbers)
        tempcorpus<-tm_map(tempcorpus,removeWords, stopwords("english"))
        tempcorpus<-tm_map(tempcorpus, stemDocument)
        
        dtm <- TermDocumentMatrix(tempcorpus)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        head(d, 10)
        print(d)
        barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
                col ="lightblue", main ="Most frequent words",
                ylab = "Word frequencies")
        
        
    })
    
    
    output$wc <- renderPlot({
        
        df <- read.csv(input$file1$datapath,
                       header = FALSE,
                       sep = ",",
                       stringsAsFactors = FALSE
        )
        
        ll<-df$V3
        
        tempcorpus = lapply(ll, tolower)
        tempcorpus<-Corpus(VectorSource(tempcorpus))
        tempcorpus<-tm_map(tempcorpus,removePunctuation)
        tempcorpus<-tm_map(tempcorpus,stripWhitespace)
        tempcorpus<-tm_map(tempcorpus,removeNumbers)
        tempcorpus<-tm_map(tempcorpus,removeWords, stopwords("english"))
        tempcorpus<-tm_map(tempcorpus, stemDocument)
        
        wordcloud(tempcorpus,scale=c(5,0.5),max.words=100,random.order=FALSE,rot.per=0.35,use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))
        #return(w)
        
    })

    output$kw <- renderTable({
        
        df <- read.csv(input$file1$datapath,
                       header = FALSE,
                       sep = ",",
                       stringsAsFactors = FALSE
        )
        yy<-paste(df[,3])
        
        keyword_list<-strsplit(input$keywd," ")[[1]]
        keyword_list<-c(keyword_list)
        
        #keyword_list = c(input$keywd)
        print("Inside Keyword_list")
        print(keyword_list)
        print(class(keyword_list))
        
        print("Inside input keyword")
        print(input$keywd)
        print(class(input$keywd))
        
        # print(yy)
        
        require(tibble)
        textdf = tibble(text = yy)
        sent_tokenized = textdf %>% unnest_tokens(sentence, text, token = "sentences")
        df_sent<-data.frame(sent_tokenized)
    
            
        d=""
        for ( i in keyword_list) {
            print("keyword of i:")
            print(i)
            d<-paste0(d,"#########################\n")
            d<-strsplit(d,"##### KEYWORD #######\n")
            c<-searchWord(yy,i)
            c<-paste0(c,i)
            c<-strsplit(c, "---------\n")[[1]]
            d<-paste0(d,c)
            
           }
        
        print(" ------------Search Sentences :-------")
        print(d)
        d<-paste0(d,"#########################\n")
        
        return(d)
        
    })
    
    output$sent <- renderTable({
        
         #df1= readLines(input$file1$datapath)
        #tmp <- sapply(reviews[,3], as.character)
        #assign("ss", "new", envir = .GlobalEnv) 
        
        df <- read.csv(input$file1$datapath,
                       header = FALSE,
                       sep = ",",
                       stringsAsFactors = FALSE
        )
        ss<-paste(df[,3])
        
        print("Inside sent tokenization")
        #print(ss)
        #p<-tokenize_sentences(ss)
        #return(tokenize_sentences(ss))
        #print(ss)
        require(tibble)
        textdf = tibble(text = ss)
        sent_tokenized = textdf %>% unnest_tokens(sentence, text, token = "sentences")
        df_sent<-data.frame(sent_tokenized)
        return(df_sent)
    })
        output$contents <- renderTable({
            
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            
            #req(input$file1)
            #ss=""
            df <- read.csv(input$file1$datapath,
                           header = FALSE,
                           sep = ",",
                           stringsAsFactors = FALSE
                          )
            ss<-paste(df[,3])
           return(ss)
        })
        
    })