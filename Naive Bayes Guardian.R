# Naïve Bayes in The Guardian Newspaper
# Political Data Science 2018
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257      201400917

# Clean list
rm(list = ls())

# Set working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

# Install necessary packages
library(dplyr)
library(tm)
library(e1071)
library(gmodels)

# Load file to work with
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian/afd_guardian_articles_full_text.RData")

# Checking my data frame
glimpse(afd)

# Randomizing my dataset
set.seed(41)
random.afd <- afd[sample(nrow(afd)), ]
random.afd <- afd[sample(nrow(afd)), ]
glimpse(random.afd)

# Converting to factor the Phases
afd$Phases <- as.factor(random.afd$Phases)

###############

                           ### Cleaning up and tokenizing the texts ###

guardian.corpus <- Corpus(VectorSource(random.afd$full_text))

guardian.clean <- guardian.corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

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

guardian.clean <- tm_map(guardian.clean, removeWords, wordstoremove)

## Create a Document Term Matrix for the words
guardian.dtm <- DocumentTermMatrix(guardian.clean)

######

    ## Randomizing my sample
    ## Organizing between train (75%) and test (25%) data

# Obs: I can do by rows because I already randomized the rows in the beginning

guardian.train <- random.afd[1:30,]
guardian.test <- random.afd[31:41,]

guardian.clean.train <- guardian.clean[1:30] 
guardian.clean.test <- guardian.clean[31:41]

guardian.dtm.train <- guardian.dtm[1:30,]
guardian.dtm.test <- guardian.dtm[31:41,]

######

    ## Selection

# Check the Frequent Terms in the DTM (the ones that appear at least 5 times)
guardian.dtm.freq <- findFreqTerms(guardian.dtm.train,5)

# Check the Length of the Frequent Terms list
length(guardian.dtm.freq) ## 257

# Create a DTM with only the frequent terms
guardian.dtm.train.nb <- DocumentTermMatrix(guardian.clean.train, 
                                            control = list(dictionary = guardian.dtm.freq))

guardian.dtm.test.nb <- DocumentTermMatrix(guardian.clean.test,
                                           control = list(dictionary = guardian.dtm.freq))


######

    ## Using Naive Bayes Algorithm for text analysis

## Convert the word frequencies to presence and absence (0 for No and 1 for Yes) 
  ## Obs: Using boolean method
guardian.convert <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels = c(0,1), labels = c("No", "Yes"))
  y
}

## Aplying the function to the DTM with only frequent terms
guardian.train.NB <- apply(guardian.dtm.train.nb, 2, guardian.convert)
guardian.test.NB <- apply(guardian.dtm.test.nb, 2, guardian.convert)

## Train the Classifier for Naive Bayes

system.time(classifier <- naiveBayes(data.frame(guardian.train.NB),y=factor(guardian.train$Phases),laplace=1))

## Testing predictions
system.time(guardian.pred <- predict(classifier, newdata=data.frame(guardian.test.NB)) )

## Creating a truth table with the predicted Phases
table("Predicted Phases" = guardian.pred, "Classified Phases" = guardian.test$Phases)

guardian.NB <- rbind(guardian.test.NB,guardian.train.NB,deparse.level = 1)
afd$Predictions = predict(classifier,newdata=data.frame(guardian.NB))

CrossTable(x = afd$Phases, y = afd$Predictions)

# output final data for AfD articles with full text and predictions
save(afd, file="predictions_guardian.RData")
