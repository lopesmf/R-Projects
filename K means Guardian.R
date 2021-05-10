# Using k-means to make clusters for The Guardian articles
# Political Data Science 2018
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257   201400917

# Clean list
rm(list = ls())

# Set working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

# Install necessary packages
library(RTextTools)
library(ggplot2)
library(cluster)
library(gmodels)

# Load file to work with
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian/predictions_guardian.RData")

##############

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

# Converting DFM to DTM
guardian.dtm <- convert(guardian.dfm, to="tm")

# Finding Frequent terms that appear at least 5 times
findFreqTerms(guardian.dtm, lowfreq = 5)

######

head(guardian.dtm)
            ## K means Analysis Clustering ##

# Create kmeans for 4 clusters
guardian.kmeans <- kmeans(guardian.dtm, 4)

# Merge the clusters as key words
guardian.kmeans$size
guardian.kmeans$centers

afd$Cluster <- guardian.kmeans$cluster


