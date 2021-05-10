# Using k-means to make clusters for Deutsche Welle articles
# Political Data Science 2018
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257   201400917

# Clean list
rm(list = ls())

# Set working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

# Install necessary packages
library(quanteda)
library(ggplot2)
library(cluster)
library(tm)
library(gmodels)

# Load file to work with
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle/predictions_deutsche.RData")

##############

### Text Organization and Formatting ###

# Extract from my dataframe, as a value, only the texts from Deutsche Welle
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

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(deutsche.dfm)

# Obs: A The first 5 words in this top features are not interesting, because all are about the main subject
# It is necessary to remove in order to check what I am really interested in

# Keep in the dfm only words that appear more than 2 articles
deutsche.dfm <- dfm_trim(deutsche.dfm, min_termfreq = 2)

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

# Converting DFM to DTM
deutsche.dtm <- convert(deutsche.dfm, to="tm")

# Finding Frequent terms that appear at least 5 times
findFreqTerms(deutsche.dtm, lowfreq = 5)

######

head(deutsche.dtm)
## K means Analysis Clustering ##

# Create kmeans for 4 clusters
deutsche.kmeans <- kmeans(deutsche.dtm, 4)

# Merge the clusters as key words
deutsche.kmeans$size
deutsche.kmeans$centers

afd$cluster <- deutsche.kmeans$cluster

# Graph some results
qplot(data = afd, 
      x = art_number, 
      y = dates, 
      color = factor(cluster), alpha = 0.5) + 
  scale_color_brewer(palette = "Dark2") + theme_bw()

CrossTable(x = afd$Phases, y = afd$cluster, prop.chisq = FALSE)

