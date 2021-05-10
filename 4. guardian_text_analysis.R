# Topic Analysis in news about AfD in The Guardian Newspaper
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257    201400917

# Clean list
rm(list = ls())

# Set Working Directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

# Install necessary packages
library(tm)
library(quanteda)

# Load data
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian/afd_guardian_articles_full_text.RData")

###############

                                    ### Organizing data ###


#Formatting dates: Transforming factor to Date
afd$my_dates <- as.Date(afd$my_dates, format = "%Y-%m-%d")

## I need to divide my data into the 4 phases identified in the literature. 
## This phases will serve as a way to analyse better the content I extracted from The Guardian website
## To have the 4 phases to analyse, I need to do two procedures.


#Selecting the phases in the timeline to do the analysis by period
phase_1 <- afd[afd$my_dates >="2013-01-01" & afd$my_dates <= "2014-05-31",]
phase_2 <- afd[afd$my_dates >="2014-06-01" & afd$my_dates <= "2015-07-30",]
phase_3 <- afd[afd$my_dates >="2015-08-01" & afd$my_dates <= "2017-09-30",]
phase_4 <- afd[afd$my_dates >="2017-10-01",]


###############

                            ### Text Organization and Formatting ###


# Extract from my dataframe, as a value, only the texts from The Guardian
articles.guardian <- gsub(":", " ", afd$full_text, fixed = T) 

# Using tokens to remove symbols, punctuation, separators, urls from the words in the texts
articles.guardian <- tokens(articles.guardian, what = "word",
                            remove_punct = T,
                            remove_symbols = T,
                            remove_separators = T,
                            remove_url = T,
                            remove_hyphens = T,
                            verbose = T)

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

# Keep in the dfm only words that appear more than 4 articles
guardian.dfm <- dfm_trim(guardian.dfm, min_termfreq = 4)

# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("afd", "germani", "german", "said", "year", "link", "post", 
                   "guardian", "merkel", "parti", "leader", "say", "AfD", "germany", "german",
                   "twitter", "tweet", "said",
                   "year", "merkel", "say",
                   "around", "although", "though", 
                   "allow", "alpha", "blame", "call",
                   "ago", "airless", "afd",
                   "found", "tag", "facebook", "instagram",
                   "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday")

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
terms(guardian.lda, 15)

#############































# randomly sample test data
set.seed(45)
select <- sample(1:nrow(guardian.dtm), size = 10)
test <- guardian.dtm[select, ]
train <- guardian.dtm[!(1:nrow(guardian.dtm) %in% select), ]

n.tops <- 2:10
metrics <- data.frame(topics = n.tops,
                      perplexity = NA)

for(i in n.tops) { # NB: takes awhile to run
  print(i)
  est <- LDA(train, k = i)
  metrics[(i - 1), "perplexity"] <- perplexity(est, newdata = test)
}

save(metrics, file = "lda_perplexity.RData")

qplot(data = metrics, x = topics, y = perplexity, geom = "line",
      xlab = "Number of topics",
      ylab = "Perplexity on test data") + theme_bw()


############################################################
## RERUN WITH BETTER CHOICE OF k
############################################################

# run lda with 10 topics
lda <- LDA(guardian.dtm, k = 10)
save(lda, file = "dr_ft_keep.RData")

# examine output
terms(lda, 10)

# put topics into original data
afd$topics <- topics(lda)

# simple frequency
qplot(data = dat, x = topic, geom = "bar")

# add labels
dat$topic_lab <- factor(dat$topic,
                        levels = 1:14,
                        labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", 
                                   "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", 
                                   "Topic 11", "Topic 12", "Topic 13", "Topic 14"))

# better frequency
qplot(data = dat, x = topic_lab, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "DR Politics topic frequencies December 2017") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

