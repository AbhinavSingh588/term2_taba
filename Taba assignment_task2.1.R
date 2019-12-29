#Task 2 - Text An of the Reviews

#check if the below packages are installed are not if not install the package in R ENVIRONMENT

try(require(tidytext) || install.packages("tidytext", dependencies = TRUE))
try(require(ggplot2) || install.packages("ggplot2", dependencies = TRUE))
try(require(dplyr) || install.packages("dplyr", dependencies = TRUE))
try(require(wordcloud) || install.packages("wordcloud", dependencies = TRUE))
try(require(stringr) || install.packages("stringr", dependencies = TRUE))
try(require(tokenizers) || install.packages("tokenizers", dependencies = TRUE))
try(require(qdap) || install.packages("qdap", dependencies = TRUE))

#Import the libraries that are used

library(tidytext)
library(ggplot2)
library(dplyr)
library(corpus)
library(wordcloud)
library(stringr)
library(tidyverse)
library(tokenizers)
library(qdap)

#selection of MG Hector car csv and loading the dataset into the R environment

Reviews <- read.csv('C:\\Users\\Meghana\\Downloads\\scrapping.csv',stringsAsFactors = FALSE)
Reviews$doc_id <- paste0('doc ',seq(1,nrow(Reviews))) # Add the new document id column
Reviews_df <- Reviews %>% select(c('doc_id','brand_name','Review')) #select only 3 columns 

head(Reviews_df)

#------------filter only mg hector reviews-----------#
text_df <- Reviews_df %>% filter(brand_name=="mg hector")
dim(text_df)
head(text_df)

#******************************************************************************************
# Text Cleaning the Reviews column content by removing the unnecessary special characters
#****************************************************************************************

text.clean <- function(x,                    # x=text_corpus
                       remove_numbers=TRUE,        # whether to drop numbers? Default is TRUE  
                       remove_stopwords=TRUE)      # whether to drop stopwords? Default is TRUE
  
{ library(tm)
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  
  if (remove_numbers) { x  =  removeNumbers(x)}    # removing numbers
  
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space. Note regex usage
  
  # evaluate  condition
  if (remove_stopwords){
    
    # read std stopwords list from sudheer's git hub account
    
    stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')
    
    # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
    stpw2 = tm::stopwords('english')      
    comn  = unique(c(stpw1, stpw2))         # Union of the two lists
    stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation
    
    # removing stopwords created above
    x  =  removeWords(x,stopwords)           }  # if condn ends
  
  x  =  stripWhitespace(x)                  # removing white space
  # x  =  stemDocument(x)                   # can stem doc if needed. For Later.
  
  return(x) }  # func ends

system.time({ text_df1 <-  text.clean(text_df, remove_numbers=FALSE) })

#***********************************************************************************
#[1] How do customers view our product against competition? 
#**********************************************************************************

system.time({
  compass_words <- text_df %>% 
    unnest_tokens(word, Review) %>%  # tokenized words in df in 'word' colm
    count(word, sort = TRUE) %>%   # counts & sorts no. of occurrences of each item in 'word' column 
    rename(count = n)      # renames the count column from 'n' (default name) to 'count'.
})

#check the top words listed in descending order of the count
compass_words %>% head(.,10) 

#Remove the stop words as they are not helpful in determining the actual features

data(stop_words)

# use anti_join() to de-merge stopwords from the df
system.time({
  
  compass_new <- text_df %>% 
    unnest_tokens(word, Review) %>% 
    count(word, sort = TRUE) %>%   
    rename(count = n) %>%
    anti_join(stop_words)   # try ?anti_join
})

#check the top 10 words after the stop words are removed

compass_new %>% head(.,10)


#building the word cloud with the compass_new
pal <- brewer.pal(4,"Dark2")
wordcloud(compass_new$word,compass_new$count,min.count = 10,max.words = 50,colors=pal,random.order = FALSE)



write.csv(text_df1, file = "my_data.txt")
