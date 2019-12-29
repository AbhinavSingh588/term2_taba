try(require(udpipe) || install.packages("udpipe", dependencies = TRUE))
try(require(sentimentr) || install.packages("sentimentr", dependencies = TRUE))


library(udpipe)
library(lattice)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(dplyr)

###2)On what attributes (e.g., price, buying experience) and product features (e.g., engine,
#interiors etc) are we perceived as Strong? Weak? 

#####Load the csv file into the R environment###########################################
Reviews <- read.csv('C:\\Users\\Meghana\\Downloads\\\\cardekhohectorscrapingtest.csv',stringsAsFactors = FALSE)
reviews_df<- Reviews %>% filter(Model=="MG HECTOR")

######Establishing an connection to udpipe###############################################
english_model = udpipe_load_model("C:\\Users\\Meghana\\Downloads\\english-ewt-ud-2.4-190531.udpipe")  # file_model only needed
x <- udpipe_annotate(english_model, x = reviews_df$Review,parser = "none",trace = FALSE) #%>% as.data.frame() %>% head()
x <- as.data.frame(x)
head(x)

#######parts of speech tagging
table(x$xpos)  ###std penn tree bank
table(x$upos) ###universal tree bank

# So what're the most common nouns? verbs?
all_nouns = x %>% subset(., upos %in% "NOUN") 
top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
head(top_nouns, 5) 

####Building a wordcloud with the top fetched nouns
pal <- brewer.pal(8,"Dark2")
wordcloud(top_nouns$key,top_nouns$freq,min.freq = 2,max.words = 50,colors=pal)

#sunroof segment,quality,feature hector are the strongest features
#

#common phrases 
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
# Building noun phrases thus (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
regexed_phrases <- keywords_phrases(x = x$phrase_tag, 
                                    term = x$token, 
                                    pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                                    is_regex = TRUE, 
                                    ngram_max = 4, 
                                    detailed = FALSE)

stats <- subset(regexed_phrases, ngram > 2)
head(stats)

head(subset(regexed_phrases, ngram > 2))

stats$keyword <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(keyword ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Features -MG HECTOR", xlab = "Frequency")

#touch screen,dream car,10 inch touch,massive panaromic sunroof are most discussed sentences


##[3] On what attributes (e.g., price, buying experience) and product features (e.g., engine,
##interiors etc) is competition perceived as Strong? Weak? 
  
#####Load the csv file into the R environment###########################################
Reviews <- read.csv('C:\\Users\\Meghana\\Downloads\\cardekhocompassscrapingtest.csv',stringsAsFactors = FALSE)
reviews_df<- Reviews %>% filter(Model=="JEEP COMPASS")

############connection to udpipe model is established###################################
english_model = udpipe_load_model("C:\\Users\\Meghana\\Downloads\\english-ewt-ud-2.4-190531.udpipe")  # file_model only needed
x <- udpipe_annotate(english_model, x = reviews_df$Review,parser = "none",trace = FALSE) #%>% as.data.frame() %>% head()
x <- as.data.frame(x)
head(x)

#######parts of speech tagging
table(x$xpos)  ###std penn tree bank
table(x$upos) ###universal tree bank

# So what're the most common nouns? verbs?
all_nouns = x %>% subset(., upos %in% "NOUN") 
top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
head(top_nouns, 5) 

pal <- brewer.pal(8,"Dark2")
wordcloud(top_nouns$key,top_nouns$freq,min.freq = 2,max.words = 50,colors=pal)
#mileage,quality,experience,vehicle,drive are some of the strongest features
#money,model,dealer,value are some of the weak or less discussed features

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
# x$phrase_tag[1:10];  x$upos[1:10];  x$token[1:10]   # uncomment & run to see what it does

# Building noun phrases thus (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
regexed_phrases <- keywords_phrases(x = x$phrase_tag, 
                                    term = x$token, 
                                    pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                                    is_regex = TRUE, 
                                    ngram_max = 4, 
                                    detailed = FALSE)

stats <- subset(regexed_phrases, ngram > 2)


head(subset(regexed_phrases, ngram > 2))

stats$keyword <- factor(stats$keyword, levels = rev(stats$keyword))

#building a ggplot based on the most discussed phrases
barchart(keyword ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Features - Jeep Compass", xlab = "Frequency")

#my jeep compass,electronic parking brake,Indian road conditions are some of the most discussed features
#good off road,good off road conditions are some of the weak features


###emotional response from the user###########
##4] What attributes or product features seem to best evoke an 'emotional' response or connect
###from customers? 
require(tidytext)
require(tidyr)
require(dplyr)
######we are using the bing method#########
###load the sentiment lexicons

sentiments <- read.csv("https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/sentiments.csv")    
sentiments

#####loading the csv file into the R environment*************************
Reviews <- read.csv('C:\\Users\\Meghana\\Downloads\\cardekhohectorscrapingtest.csv',stringsAsFactors = FALSE)
Reviews$doc_id <- paste0('doc ',seq(1,nrow(Reviews))) # Add the new document id column
Reviews_df <- Reviews %>% select(c('doc_id','Model','Review')) #select only 3 columns 

head(Reviews_df)

#------------filter only mg hector reviews-----------#
text_df <- Reviews_df %>% filter(Model == "MG HECTOR")
dim(text_df)
head(text_df)


textdf <- data_frame(text = text_df)

bing <- get_sentiments("bing")   # put all of the bing sentiment dict into object 'bing'
bing  

senti.bing <- text_df %>%
  mutate(linenumber = seq(1:nrow(text_df))) %>%   # build line num variable
  ungroup() %>%
  unnest_tokens(word, Review) %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%
  # mutate(index = row_number()) %>%
  mutate(method = "bing") 

senti.bing
#using the spread functionto combine extra rows pertaining to some index
bing_df <- data.frame(senti.bing %>% spread(sentiment, n, fill = 0))

head(bing_df)

#calculating the overall polarity 

bing_pol = bing_df %>% 
  mutate(polarity = (positive - negative)) %>%   #create variable polarity = pos - neg
  arrange(desc(polarity))    # sort by polarity

bing_pol %>%  head()

# plotting running sentiment distribution across the analyst call
ggplot(bing_pol, 
       aes(index, polarity)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Sentiment in Car Reviews",
       x = "doc",  
       y = "Sentiment")

#checking which are the most p[ositive and the negitive points
bing_word_counts <- text_df %>%
  unnest_tokens(word, Review) %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

#ggplot to find the most discussed positive and negitive emotions along with the axis text

bing_word_counts %>%
  filter(n > 3) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#good,best like are the most positive emotions specified  by the user 
#problem and lack are the most negitive words specified by the user

#####[5] How should we position ourselves and promote our product against competition?#####
##### As specified by the customers the most discussed feature is the 10 inch touch screen and
###### massive panaromic sunroof for the mg hector which would also be promoted using these features
###### as well as trying to add the feature of electronic parking brake which is the most discussed 
###### feature of jeep compass in the next model which they are trying to build
#####mg hector car 