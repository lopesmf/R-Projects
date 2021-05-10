# Naïve Bayes in Deutsche Welle Newspaper
# Political Data Science 2018
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257      201400917

# Clean list
rm(list = ls())

# Set working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

# Install necessary packages
library(dplyr)
library(tm)
library(e1071)
library(gmodels)

# Load file to work with
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle/afd_deutsche_articles_full_text.RData")

# Checking my data frame
glimpse(afd)

# Randomizing my dataset
set.seed(344)
random.afd <- afd[sample(nrow(afd)), ]
glimpse(random.afd)

# Converting to factor the Phases
afd$Phases <- as.factor(random.afd$Phases)

###############

### Cleaning up and tokenizing the texts ###

deutsche.corpus <- Corpus(VectorSource(random.afd$full_text))

deutsche.clean <- deutsche.corpus %>%
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

deutsche.clean <- tm_map(deutsche.clean, removeWords, wordstoremove)

## Create a Document Term Matrix for the words
deutsche.dtm <- DocumentTermMatrix(deutsche.clean)

######

## Randomizing my sample
## Organizing between train (75%) and test (25%) data

# Obs: I can do by rows because I already randomized the rows in the beginning

deutsche.train <- random.afd[1:258,]
deutsche.test <- random.afd[259:344,]

deutsche.clean.train <- deutsche.clean[1:258] 
deutsche.clean.test <- deutsche.clean[259:344]

deutsche.dtm.train <- deutsche.dtm[1:258,]
deutsche.dtm.test <- deutsche.dtm[259:344,]

######

## Selection

# Check the Frequent Terms in the DTM (the ones that appear at least 5 times)
deutsche.dtm.freq <- findFreqTerms(deutsche.dtm.train,5)

# Check the Length of the Frequent Terms list
length(deutsche.dtm.freq) ## 3292

# Create a DTM with only the frequent terms
deutsche.dtm.train.nb <- DocumentTermMatrix(deutsche.clean.train, 
                                            control = list(dictionary = deutsche.dtm.freq))

deutsche.dtm.test.nb <- DocumentTermMatrix(deutsche.clean.test,
                                           control = list(dictionary = deutsche.dtm.freq))


######

## Using Naive Bayes Algorithm for text analysis

## Convert the word frequencies to presence and absence (0 for No and 1 for Yes) 
## Obs: Using boolean method
deutsche.convert <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels = c(0,1), labels = c("No", "Yes"))
  y
}

## Aplying the function to the DTM with only frequent terms
deutsche.train.NB <- apply(deutsche.dtm.train.nb, 2, deutsche.convert)
deutsche.test.NB <- apply(deutsche.dtm.test.nb, 2, deutsche.convert)

## Train the Classifier for Naive Bayes

system.time(classifier <- naiveBayes(data.frame(deutsche.train.NB),y=factor(deutsche.train$Phases),laplace=1))

## Testing predictions
system.time(deutsche.pred <- predict(classifier, newdata=data.frame(deutsche.test.NB)) )

## Creating a truth table with the predicted Phases
table("Predicted Phases" = deutsche.pred, "Classified Phases" = deutsche.test$Phases)

deutsche.NB <- rbind(deutsche.test.NB,deutsche.train.NB,deparse.level = 1)
afd$Predictions = predict(classifier,newdata=data.frame(deutsche.NB))

CrossTable(x = afd$Phases, y = afd$Predictions)
# output final data for AfD articles with full text and predictions
save(afd, file="predictions_deutsche.RData")
