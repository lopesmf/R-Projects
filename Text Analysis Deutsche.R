# Text Analysis of AfD articles in Deutsche Welle Newspaper
# Political Data Science 2018
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257      201400917

# Clean list
rm(list = ls())

# Set working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

# Install necessary packages
library(tm)
library(quanteda)
library(topicmodels)
library(ggplot2)

# Load file to work with
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle/predictions_deutsche.RData")

###############

                                ### Organizing data ###

## I need to divide my data into the 4 phases identified in the literature. 
## This phases will serve as a way to analyse better the content I extracted from The deutsche website
## To have the 4 phases to analyse, I need to do two procedures.

#Formatting dates: Transforming factor to Date
afd$dates <- as.Date(afd$dates, format = "%Y-%m-%d")

#Selecting the phases in the timeline to do the analysis by period
phase_1 <- afd[afd$dates >="2013-01-01" & afd$dates <= "2014-05-31",]
phase_2 <- afd[afd$dates >="2014-06-01" & afd$dates <= "2015-07-30",]
phase_3 <- afd[afd$dates >="2015-08-01" & afd$dates <= "2017-09-30",]
phase_4 <- afd[afd$dates >="2017-10-01",]


###############

                     ### Text Organization and Formatting ###

# Extract from my dataframe, as a value, only the texts from The deutsche
quanteda_options("language_stemmer" = "english")
articles.deutsche <- gsub(":", " ", afd$full_text, fixed = T) 

# Using tokens to remove symbols, punctuation, separators, urls from the words in the texts
articles.deutsche <- tokens(articles.deutsche, what = "word",
                            remove_punct = T,
                            remove_symbols = T,
                            remove_separators = T,
                            remove_url = T,
                            remove_hyphens = T,
                            verbose = T,
                            remove_twitter = T,
                            remove_numbers = T)

# Transforming all words to lower case
articles.deutsche <- tokens_tolower(articles.deutsche) 

# Removing english stopwords
articles.deutsche <- tokens_remove(articles.deutsche, stopwords("english")) 

# Stem words
articles.deutsche <- tokens_wordstem(articles.deutsche)

# Remove stopwords after stem the text
articles.deutsche <- tokens_remove(articles.deutsche, stopwords("english")) 

# Creating a dfm (document feature matrix) with the tokenized articles
deutsche.dfm <- dfm(articles.deutsche)


#############


                                ##### 1st Analysis #####


#### Main Goal: Analyse the text from the entire data set from The deutsche
### After Procedure: Compare to the results I will get by analysing the text phase by phase

## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(deutsche.dfm)

df <- data.frame(word = names(topfeatures(deutsche.dfm)), count = topfeatures(deutsche.dfm))

# Ggplot about the world count
ggplot(df,
       aes(x = reorder(df$word, df$count), y = df$count)) +
  geom_col(stat = "identity", fill = c("coral3", "steelblue", "darkgoldenrod", "forestgreen", "bisque3", "deepskyblue4", "darksalmon", "darkseagreen", "burlywood4", "brown4")) +
  coord_flip() +
  xlab("Count") + ylab("Words") +
  ggtitle("Top Features Deutsche Welle Words") +
  theme_minimal()

# Obs: A The first 5 words in this top features are not interesting, because all are about the main subject
# It is necessary to remove in order to check what I am really interested in

# Keep in the dfm only words that appear more than 2 articles
deutshce.dfm <- dfm_trim(deutsche.dfm, min_termfreq = 2)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "deutsche", "merkel", "parti", "leader", "say", "AfD", "germany", "german",
                   "twitter", "tweet", "said",
                   "year", "merkel", "say",
                   "around", "although", "though", 
                   "allow", "alpha", "blame", "call",
                   "ago", "airless", "afd",
                   "found", "tag", "facebook", "instagram",
                   "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday",
                   "altern", "group", "first", "second", "third", "last",
                   "two", "one", "three", "want", "von", "came", "told",
                   "write", "wrote", "tell", "just", "sinc", "since", "also", "further",
                   "before", "after", "percent", "report", "will", "be", "journalist", "thing",
                   "angela", "deutschland", "für")

deutsche.dfm <- dfm_remove(deutsche.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(deutsche.dfm)



      ## Selecting features by importance in a document ##


# Create tf_idf-weighted dfm 
# (The Tf-idf is the frequency of a term adjusted for how rarely it is used)
deutsche.tfidf <- dfm_tfidf(deutsche.dfm)

# Select from main dfm using its top features
deutsche.dfm <- dfm_keep(deutsche.dfm, names(topfeatures(deutsche.tfidf, n = 1000)))


      ## Run the topic models ## 


# Goal: Want to observe the words appearing in the topic models, where I have 4 randomized clusters

# Convert 'quanteda dfm' to 'tm dtm' #better to run the topic models
deutsche.dtm <- convert(deutsche.dfm, to = "topicmodels")

# Using Latent Dirichlet Allocation (LDA) to run the topic model (Choice of 4 clusters)
deutsche.lda <- LDA(deutsche.dtm, k = 4)

# review terms by topic
terms(deutsche.lda, 20)
matrix(data = terms(deutsche.lda, 15), nrow = 15, ncol = 4, byrow = FALSE)

# Create a column with the topics in the original data frame
afd$topic <- topics(deutsche.lda)

# Plot the frequencies for 4 clusters
qplot(data = afd, x = topic, geom = "bar")

# Adding labels to the original dataframe
afd$Labels <- factor(afd$topic,
                     levels = 1:4,
                     labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4"))

# Frequency using 4 random clusters
qplot(data = afd, x = Labels,
      geom = "bar", xlab = "", 
      ylab = "Frequency",
      main = "Topic Frequencies in AfD Articles in Deutsche Welle, using 4 clusters") +
  theme_minimal()+
  geom_bar(stat = "count", fill = c("coral3", "steelblue", "darkgoldenrod", "forestgreen"))+
  theme(axis.text.x = element_text(angle = 90))


### Looking for the optimal k

# randomly sample test data
set.seed(344)
select <- sample(1:nrow(deutsche.dtm), size = 250)
test <- deutsche.dtm[select, ]
train <- deutsche.dtm[!(1:nrow(deutsche.dtm) %in% select), ]

n.tops <- 2:15
metrics <- data.frame(topics = n.tops,
                      perplexity = NA)

for(i in n.tops) { # NB: takes awhile to run
  print(i)
  est <- LDA(train, k = i)
  metrics[(i - 1), "perplexity"] <- perplexity(est, newdata = test)
}


qplot(data = metrics, x = topics, y = perplexity, geom = "line",
      xlab = "Number of topics",
      ylab = "Perplexity on test data") + theme_bw()

    ## Run with a better k [The result in the plot was 2]

# Run lda with 2 topics
deutsche.lda <- LDA(deutsche.dtm, k = 2)

# Examine output
terms(deutsche.lda, 20)

# put topics into original data
afd$topic <- topics(deutsche.lda)

# simple frequency
qplot(data = afd, x = topic, geom = "bar")

# add labels
afd$topic_lab <- factor(afd$topic,
                        levels = 1:2,
                        labels = c("Topic 1", "Topic 2"))

# better frequency
qplot(data = afd, x = topic_lab, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Topic Frequencies in AfD Articles in Deutsche Welle") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#################


                              ### Analysis of Phase 1 ###


phase1.deutsche <- articles.deutsche[341:344]

# Creating a dfm (document feature matrix) with the tokenized articles
phase1.deutsche.dfm <- dfm(phase1.deutsche)


#### Main Goal: Analyse the articles for Phase 1


    ## Top Features Observation ##


# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase1.deutsche.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase1.deutsche.dfm <- dfm_trim(phase1.deutsche.dfm, min_termfreq = 2)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "deutsche", "merkel", "parti", "leader", "say", "AfD", "germany", "german",
                   "twitter", "tweet", "said",
                   "year", "merkel", "say",
                   "around", "although", "though", 
                   "allow", "alpha", "blame", "call",
                   "ago", "airless", "afd",
                   "found", "tag", "facebook", "instagram",
                   "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday",
                   "altern", "group", "first", "second", "third", "last",
                   "two", "one", "three", "want", "von", "came", "told",
                   "write", "wrote", "tell", "just", "sinc", "since", "also", "further",
                   "before", "after", "percent", "report", "will", "be", "journalist", "thing",
                   "angela", "deutschland", "für")

phase1.deutsche.dfm <- dfm_remove(phase1.deutsche.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase1.deutsche.dfm)

   
#################

                          ### Analysis of Phase 2 ###


phase2.deutsche <- articles.deutsche[321:340]

# Creating a dfm (document feature matrix) with the tokenized articles
phase2.deutsche.dfm <- dfm(phase2.deutsche)


    #### Main Goal: Analyse the articles for Phase 2


## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase2.deutsche.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase2.deutsche.dfm <- dfm_trim(phase2.deutsche.dfm, min_termfreq = 2)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "deutsche", "merkel", "parti", "leader", "say", "AfD", "germany", "german",
                   "twitter", "tweet", "said",
                   "year", "merkel", "say",
                   "around", "although", "though", 
                   "allow", "alpha", "blame", "call",
                   "ago", "airless", "afd",
                   "found", "tag", "facebook", "instagram",
                   "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday",
                   "altern", "group", "first", "second", "third", "last",
                   "two", "one", "three", "want", "von", "came", "told",
                   "write", "wrote", "tell", "just", "sinc", "since", "also", "further",
                   "before", "after", "percent", "report", "will", "be", "journalist", "thing",
                   "angela", "deutschland", "für")

phase2.deutsche.dfm <- dfm_remove(phase2.deutsche.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase2.deutsche.dfm)

#################


                          ### Analysis of Phase 3 ###

phase3.deutsche <- articles.deutsche[109:320]

# Creating a dfm (document feature matrix) with the tokenized articles
phase3.deutsche.dfm <- dfm(phase3.deutsche)


    #### Main Goal: Analyse the articles for Phase 3


## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase3.deutsche.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase3.deutsche.dfm <- dfm_trim(phase3.deutsche.dfm, min_termfreq = 2)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "deutsche", "merkel", "parti", "leader", "say", "AfD", "germany", "german",
                   "twitter", "tweet", "said",
                   "year", "merkel", "say",
                   "around", "although", "though", 
                   "allow", "alpha", "blame", "call",
                   "ago", "airless", "afd",
                   "found", "tag", "facebook", "instagram",
                   "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday",
                   "altern", "group", "first", "second", "third", "last",
                   "two", "one", "three", "want", "von", "came", "told",
                   "write", "wrote", "tell", "just", "sinc", "since", "also", "further",
                   "before", "after", "percent", "report", "will", "be", "journalist", "thing",
                   "angela", "deutschland", "für")

phase3.deutsche.dfm <- dfm_remove(phase3.deutsche.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase3.deutsche.dfm)


#################


                            ### Analysis of Phase 4 ###


phase4.deutsche <- articles.deutsche[1:108]

# Creating a dfm (document feature matrix) with the tokenized articles
phase4.deutsche.dfm <- dfm(phase4.deutsche)


      #### Main Goal: Analyse the articles for Phase 4


## Top Features Observation ##


# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase4.deutsche.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase4.deutsche.dfm <- dfm_trim(phase4.deutsche.dfm, min_termfreq = 2)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "deutsche", "merkel", "parti", "leader", "say", "AfD", "germany", "german",
                   "twitter", "tweet", "said",
                   "year", "merkel", "say",
                   "around", "although", "though", 
                   "allow", "alpha", "blame", "call",
                   "ago", "airless", "afd",
                   "found", "tag", "facebook", "instagram",
                   "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday",
                   "altern", "group", "first", "second", "third", "last",
                   "two", "one", "three", "want", "von", "came", "told",
                   "write", "wrote", "tell", "just", "sinc", "since", "also", "further",
                   "before", "after", "percent", "report", "will", "be", "journalist", "thing",
                   "angela", "deutschland", "für")

phase4.deutsche.dfm <- dfm_remove(phase4.deutsche.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase4.deutsche.dfm)

