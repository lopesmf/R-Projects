# Text Analysis of AfD articles in The Guardian Newspaper
# Political Data Science 2018
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257      201400917

# Clean list
rm(list = ls())

# Set working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

# Install necessary packages
library(tm)
library(quanteda)
library(topicmodels)
library(ggplot2)
library(wordcloud)

# Load file to work with
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian/predictions_guardian.RData")

###############

                                        ### Organizing data ###

## I need to divide my data into the 4 phases identified in the literature. 
## This phases will serve as a way to analyse better the content I extracted from The Guardian website
## To have the 4 phases to analyse, I need to do two procedures.

#Formatting dates: Transforming factor to Date
afd$my_dates <- as.Date(afd$my_dates, format = "%Y-%m-%d")

#Selecting the phases in the timeline to do the analysis by period
phase_1 <- afd[afd$my_dates >="2013-01-01" & afd$my_dates <= "2014-05-31",]
phase_2 <- afd[afd$my_dates >="2014-06-01" & afd$my_dates <= "2015-07-30",]
phase_3 <- afd[afd$my_dates >="2015-08-01" & afd$my_dates <= "2017-09-30",]
phase_4 <- afd[afd$my_dates >="2017-10-01",]


###############

                                ### Text Organization and Formatting ###


# Extract from my dataframe, as a value, only the texts from The Guardian
quanteda_options("language_stemmer" = "english")
articles.guardian <- gsub(":", " ", afd$full_text, fixed = T) 

# Using tokens to remove symbols, punctuation, separators, urls from the words in the texts
articles.guardian <- tokens(articles.guardian, what = "word",
                            remove_punct = T,
                            remove_symbols = T,
                            remove_separators = T,
                            remove_url = T,
                            remove_hyphens = T,
                            verbose = T,
                            remove_twitter = T,
                            remove_numbers = T)

# Transforming all words to lower case
articles.guardian <- tokens_tolower(articles.guardian) 

# Removing english stopwords
articles.guardian <- tokens_remove(articles.guardian, stopwords("english")) 

# Stem words
articles.guardian <- tokens_wordstem(articles.guardian)

# Remove stopwords after stem the text
articles.guardian <- tokens_remove(articles.guardian, stopwords("english")) 

# Creating a dfm (document feature matrix) with the tokenized articles
guardian.dfm <- dfm(articles.guardian)


#############


                                    ##### 1st Analysis #####

#### Main Goal: Analyse the text from the entire data set from The Guardian
### After Procedure: Compare to the results I will get by analysing the text phase by phase

      ## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(guardian.dfm)

  # Obs: A The first 5 words in this top features are not interesting, because all are about the main subject
  # It is necessary to remove in order to check what I am really interested in

# Keep in the dfm only words that appear more than 2 articles
guardian.dfm <- dfm_trim(guardian.dfm, min_termfreq = 2)

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

guardian.dfm <- dfm_remove(guardian.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(guardian.dfm)

## Selecting features by importance in a document ##

# Create tf_idf-weighted dfm 
# (The Tf-idf is the frequency of a term adjusted for how rarely it is used)
guardian.tfidf <- dfm_tfidf(guardian.dfm)

# Select from main dfm using its top features
guardian.dfm <- dfm_keep(guardian.dfm, names(topfeatures(guardian.tfidf, n = 1000)))


      ## Run the topic models ## 


  # Goal: Want to observe the words appearing in the topic models, where I have 4 randomized clusters

# Convert 'quanteda dfm' to 'tm dtm' #better to run the topic models
guardian.dtm <- convert(guardian.dfm, to = "topicmodels")

# Using Latent Dirichlet Allocation (LDA) to run the topic model (Choice of 4 clusters)
guardian.lda <- LDA(guardian.dtm, k = 4)

# review terms by topic
terms(guardian.lda, 20)
guardian.mat <- matrix(data = terms(guardian.lda, 20), nrow = 20, ncol = 4, byrow = FALSE)


# Create a column with the topics in the original data frame
afd$topic <- topics(guardian.lda)

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
      main = "Topic Frequencies in AfD Articles in The Guardian, using 4 clusters") +
  theme_minimal()+
  geom_bar(stat = "count", fill = c("coral3", "steelblue", "darkgoldenrod", "forestgreen"))+
  theme(axis.text.x = element_text(angle = 90))


# Analysis table
analy_table.guardian <- data.frame(afd$art_number, afd$Labels)
analy_table.guardian$afd.art_number <- paste0("Art ", seq.int(nrow(analy_table.guardian)))

analy_table.guardian$phases <- afd$Phases


    ### Looking for the optimal k

# randomly sample test data
set.seed(41)
select <- sample(1:nrow(guardian.dtm), size = 30)
test <- guardian.dtm[select, ]
train <- guardian.dtm[!(1:nrow(guardian.dtm) %in% select), ]

n.tops <- 2:15
metrics <- data.frame(topics = n.tops,
                      perplexity = NA)

for(i in n.tops) {
  print(i)
  est <- LDA(train, k = i)
  metrics[(i - 1), "perplexity"] <- perplexity(est, newdata = test)
}


qplot(data = metrics, x = topics, y = perplexity, geom = "line",
      xlab = "Number of topics",
      ylab = "Perplexity on test data") + theme_bw()


    ## Re run with the optimal k from the previous qplot

# Run LDA with 11 topics
guardian.lda <- LDA(guardian.dtm, k = 11)

# Examine output
terms(guardian.lda, 10)

# Put the topics in the original data
afd$topic <- topics(guardian.lda)

# Frequency of the topics
qplot(data = afd, x = topic, geom = "bar")

# Adding labels to the frequencies
afd$topic_lab <- factor(afd$topic,
                        levels = 1:11,
                        labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", 
                                   "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", 
                                   "Topic 11"))

# Plot the Frequency of Topics with the labels
qplot(data = afd, x = topic_lab, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Topic Frequencies in AfD Articles in The Guardian") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


#################

                                  ### Analysis of Phase 2 ###


phase2.guardian <- articles.guardian[40:41]

# Creating a dfm (document feature matrix) with the tokenized articles
phase2.guardian.dfm <- dfm(phase2.guardian)

#### Main Goal: Analyse the articles for Phase 2


      ## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase2.guardian.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase2.guardian.dfm <- dfm_trim(phase2.guardian.dfm, min_termfreq = 2)

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
                   "before", "after", "percent", "report", "will","be", "journalist", "thing", "angela", "deutschland", "für")

phase2.guardian.dfm <- dfm_remove(phase2.guardian.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase2.guardian.dfm)

#################

                                  ### Analysis of Phase 3 ###

phase3.guardian <- articles.guardian[13:39]

# Creating a dfm (document feature matrix) with the tokenized articles
phase3.guardian.dfm <- dfm(phase3.guardian)

#### Main Goal: Analyse the articles for Phase 3

        ## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase3.guardian.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase3.guardian.dfm <- dfm_trim(phase3.guardian.dfm, min_termfreq = 2)

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
                   "before", "after", "percent", "report", "will","be" , "journalist", "thing", 
                   "angela", "deutschland", "für")

phase3.guardian.dfm <- dfm_remove(phase3.guardian.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase3.guardian.dfm)

#################

                                  ### Analysis of Phase 4 ###

phase4.guardian <- articles.guardian[1:12]

# Creating a dfm (document feature matrix) with the tokenized articles
phase4.guardian.dfm <- dfm(phase4.guardian)

#### Main Goal: Analyse the articles for Phase 4

      ## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(phase4.guardian.dfm)

# Keep in the dfm only words that appear more than 2 articles
phase4.guardian.dfm <- dfm_trim(phase4.guardian.dfm, min_termfreq = 2)

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

phase4.guardian.dfm <- dfm_remove(phase4.guardian.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(phase4.guardian.dfm)

