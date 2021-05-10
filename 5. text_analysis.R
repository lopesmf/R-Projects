# Text analysis from The Guardian Afd articles
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#Clean list
rm(list = ls())

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

#install necessary packages
library(tm)
library(SnowballC)
library(quanteda)
library(wordcloud)
library(stringr)

#load articles data
load("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle/afd_deutsche_articles_full_text.RData")

afd$dates <- as.Date(afd$dates, format = "%Y-%m-%d")
phase_1 <- afd[afd$dates >="2013-01-01" & afd$dates <= "2014-05-31",]
phase_2 <- afd[afd$dates >="2014-06-01" & afd$dates <= "2015-07-30",]
phase_3 <- afd[afd$dates >="2015-08-01" & afd$dates <= "2017-09-30",]
phase_4 <- afd[afd$dates >="2017-10-01",]

##########Creating for the total document##########

## 'tokenize' fulltext
quanteda_options("language_stemmer" = "english")
texts <- gsub(":", " ", afd$full_text, fixed = T)

texts <- tokens(texts, what = "word",
                remove_numbers = T,
                remove_punct = T,
                remove_symbols = T,
                remove_separators = T,
                remove_hyphens = T,
                remove_url = T,
                verbose = T)

texts <- tokens_tolower(texts)
texts <- tokens_remove(texts, stopwords("english"))
texts <- tokens_wordstem(texts)
texts <- tokens_remove(texts, stopwords("english"))

# get actual dfm from tokens
txt.mat <- dfm(texts)

# check out top-appearing features in dfm
topfeatures(txt.mat, n = 200)
names(topfeatures(txt.mat, n = 300))

# keep features (words) appearing in >2 documents
txt.mat <- txt.mat[, colSums(txt.mat) > 2]

# filter out one-character words
txt.mat <- txt.mat[, str_length(colnames(txt.mat)) > 1]

head(txt.mat)

# check out top-appearing features in dfm
topfeatures(txt.mat, n = 20)
names(topfeatures(txt.mat, n = 300))

#####

##Creating a matrix from the articles data for phase 1
## 'tokenize' fulltext
quanteda_options("language_stemmer" = "english")
texts1 <- gsub(":", " ", phase_1$full_text, fixed = T)

texts1 <- tokens(texts1, what = "word",
                 remove_numbers = T,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_separators = T,
                 remove_hyphens = T,
                 remove_url = T,
                 verbose = T)

texts1 <- tokens_tolower(texts1)
text1 <- tokens_remove(texts1, stopwords("english"))
texts1 <- tokens_wordstem(texts1)
texts1 <- tokens_remove(texts1, stopwords("english"))

# get actual dfm from tokens
txt.mat1 <- dfm(texts1)

# check out top-appearing features in dfm
topfeatures(txt.mat1, n = 200)
names(topfeatures(txt.mat1, n = 300))

# keep features (words) appearing in >2 documents
txt.mat1 <- txt.mat1[, colSums(txt.mat1) > 2]

# filter out one-character words
txt.mat1 <- txt.mat1[, str_length(colnames(txt.mat1)) > 1]

head(txt.mat1)

# check out top-appearing features in dfm
topfeatures(txt.mat1, n = 20)
names(topfeatures(txt.mat1, n = 300))

########

##Creating a matrix from the articles data for phase 2
## 'tokenize' fulltext
quanteda_options("language_stemmer" = "english")
texts2 <- gsub(":", " ", phase_2$full_text, fixed = T)

texts2 <- tokens(texts2, what = "word",
                 remove_numbers = T,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_separators = T,
                 remove_hyphens = T,
                 remove_url = T,
                 verbose = T)

texts2 <- tokens_tolower(texts2)
text2 <- tokens_remove(texts2, stopwords("english"))
texts2 <- tokens_wordstem(texts2)
texts2 <- tokens_remove(texts2, stopwords("english"))

# get actual dfm from tokens
txt.mat2 <- dfm(texts2)

# check out top-appearing features in dfm
topfeatures(txt.mat2, n = 200)
names(topfeatures(txt.mat2, n = 300))

# keep features (words) appearing in >2 documents
txt.mat2 <- txt.mat2[, colSums(txt.mat2) > 2]

# filter out one-character words
txt.mat2 <- txt.mat2[, str_length(colnames(txt.mat2)) > 1]

head(txt.mat2)

# check out top-appearing features in dfm
topfeatures(txt.mat2, n = 200)
names(topfeatures(txt.mat2, n = 300))

#######

##Creating a matrix from the articles data for phase 3
## 'tokenize' fulltext
quanteda_options("language_stemmer" = "english")
texts3 <- gsub(":", " ", phase_3$full_text, fixed = T)

texts3 <- tokens(texts3, what = "word",
                 remove_numbers = T,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_separators = T,
                 remove_hyphens = T,
                 remove_url = T,
                 verbose = T)

texts3 <- tokens_tolower(texts3)
text3 <- tokens_remove(texts3, stopwords("english"))
texts3 <- tokens_wordstem(texts3)
texts3 <- tokens_remove(texts3, stopwords("english"))

# get actual dfm from tokens
txt.mat3 <- dfm(texts3)

# check out top-appearing features in dfm
topfeatures(txt.mat3, n = 200)
names(topfeatures(txt.mat3, n = 300))

# keep features (words) appearing in >2 documents
txt.mat3 <- txt.mat3[, colSums(txt.mat3) > 2]

# filter out one-character words
txt.mat3 <- txt.mat3[, str_length(colnames(txt.mat3)) > 1]

head(txt.mat3)

# check out top-appearing features in dfm
topfeatures(txt.mat3, n = 200)
names(topfeatures(txt.mat3, n = 300))

##########

##Creating a matrix from the articles data for phase 4
## 'tokenize' fulltext
quanteda_options("language_stemmer" = "english")
texts4 <- gsub(":", " ", phase_4$full_text, fixed = T)

texts4 <- tokens(texts4, what = "word",
                 remove_numbers = T,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_separators = T,
                 remove_hyphens = T,
                 remove_url = T,
                 verbose = T)

texts4 <- tokens_tolower(texts4)
text4 <- tokens_remove(texts4, stopwords("english"))
texts4 <- tokens_wordstem(texts4)
texts4 <- tokens_remove(texts4, stopwords("english"))

# get actual dfm from tokens
txt.mat4 <- dfm(texts4)

# check out top-appearing features in dfm
topfeatures(txt.mat4, n = 200)
names(topfeatures(txt.mat4, n = 300))

# keep features (words) appearing in >2 documents
txt.mat4 <- txt.mat4[, colSums(txt.mat4) > 2]

# filter out one-character words
txt.mat4 <- txt.mat4[, str_length(colnames(txt.mat4)) > 1]

head(txt.mat4)

# check out top-appearing features in dfm
topfeatures(txt.mat4, n = 200)
names(topfeatures(txt.mat4, n = 300))
