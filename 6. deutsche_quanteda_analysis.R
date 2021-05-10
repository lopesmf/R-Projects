# Text analysis: Analysing articles about the AfD Party in Deutsche Welle Newspaper
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#Clean Lists
rm(list = ls())

#Set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

#Install necessary packages
library(dplyr)
library(quanteda)
library(topicmodels)
library(ggplot2)

#Load data
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle/afd_deutsche_articles_full_text.RData")

## Organizing data

#Formatting dates: Transforming factor to Date
afd$dates <- as.Date(afd$dates, format = "%Y-%m-%d")

#Removing rows without text in the column full_text
afd <- afd %>%
  filter(grepl(" ", full_text, ignore.case = TRUE))

#############

## I need to divide my data into the 4 phases identified in the literature. 
## This phases will serve as a way to analyse better the content I extracted from The Guardian website
## To have the 4 phases to analyse, I need to do two procedures.

#Selecting the phases in the timeline to do the analysis by period
phase_1 <- afd[afd$dates >="2013-01-01" & afd$dates <= "2014-05-31",]
phase_2 <- afd[afd$dates >="2014-06-01" & afd$dates <= "2015-07-30",]
phase_3 <- afd[afd$dates >="2015-08-01" & afd$dates <= "2017-09-30",]
phase_4 <- afd[afd$dates >="2017-10-01",]

#############

### Creating a matrix for the articles data ###

# Tokenize full text
quanteda_options("language_stemmer" = "english")
articles.deutsche <- gsub(":", " ", afd$full_text, fixed = T) ##This will extract from my dataframe, as a value, only the texts from The Guardian

# Using tokens to remove symbols, punctuation, separators, urls from the words in the texts
articles.deutsche <- tokens(articles.deutsche, what = "word",
                            remove_punct = T,
                            remove_symbols = T,
                            remove_separators = T,
                            remove_url = T,
                            remove_hyphens = T,
                            remove_numbers = T,
                            verbose = T)

articles.deutsche <- tokens_tolower(articles.deutsche) #transforming all words to lower case
articles.deutsche <- tokens_remove(articles.deutsche, stopwords("english")) #removing english stopwords
articles.deutsche <- tokens_wordstem(articles.deutsche) #steam words
articles.deutsche <- tokens_remove(articles.deutsche, stopwords("english")) #removing stopwords after steam the text

# Creating a dfm (document feature matrix) with the tokenized articles
deutsche.dfm <- dfm(articles.deutsche)

#############

                          ##### 1st Analysis #####

#### Main Goal: Analyse the text from the entire data set from Deutsche Welle
### After Procedure: Compare to the results I will get by analysing the text phase by phase

## Top Features Observation ##

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(deutsche.dfm)

# Keep in the dfm only words that appear more than 4 articles
deutsche.dfm <- dfm_trim(deutsche.dfm, min_termfreq = 4)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "deutsche", "merkel", "parti", "leader", "say", "also")
deutsche.dfm <- dfm_remove(deutsche.dfm, wordstoremove)

#Checking the top features after removing non-wanted words
topfeatures(deutsche.dfm)

## Selecting features by importance in a document ##

# Create tf_idf-weighted dfm 
# (The Tf-idf is the frequency of a term adjusted for how rarely it is used)
deutsche.tfidf <- dfm_tfidf(deutsche.dfm)

# Select from main dfm using its top features
deutsche.dfm <- dfm_keep(deutsche.dfm, names(topfeatures(deutsche.tfidf, n = 2000)))

## Run the topic models ## 

# Goal: Want to observe the words appearing in the topic models, where I have 4 randomized clusters

# Convert 'quanteda dfm' to 'tm dtm' #better to run the topic models
deutsche.dtm <- convert(deutsche.dfm, to = "topicmodels")

# Using Latent Dirichlet Allocation (LDA) to run the topic model (Choice of 4 clusters)
deutsche.lda <- LDA(deutsche.dtm, k = 4)

# review terms by topic
terms(deutsche.lda, 15)

#############

            #### Getting the better k ####

# Randomize my data
set.seed(344)
select <- sample(1:nrow(deutsche.dtm), size = 172)
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


############################################################
## RERUN WITH BETTER CHOICE OF k
############################################################

# run lda with 10 topics
deutsche.lda <- LDA(deutsche.dtm, k = 4)

# examine output
terms(deutsche.lda, 20)

# put topics into original data
afd$topics <- topics(deutsche.lda)

# simple frequency
qplot(data = afd, x = topics, geom = "bar")

# add labels
afd$topic_lab <- factor(afd$topics,
                        levels = 1:4,
                        labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4"))

# better frequency
qplot(data = afd, x = topic_lab, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Topic Frequencies in Deutsche Welle articles about AfD Party") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


