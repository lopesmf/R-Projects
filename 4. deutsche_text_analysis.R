#Text analysis from Deutche Welle Afd articles
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

#install necessary packages
library(tm)
library(SnowballC)
library(quanteda)
library(wordcloud)

#Loading the raw texts in txt format
deutsche.afd.raw <- Corpus(DirSource(directory = "deutsche_txt", pattern = "art"))

##Clean my data from non-wanted objects, also letting all the text in lower case and without punctuation
##Goal: To facilitate the text analysis with the objects that are really necessary

deutsche.afd.prep <- tm_map(deutsche.afd.raw, content_transformer(tolower)) #Transforming to lower case
deutsche.afd.prep <- tm_map(deutsche.afd.raw, stripWhitespace) #To remove white spaces
deutsche.afd.prep <- tm_map(deutsche.afd.raw, removePunctuation) #To remove punctuation
deutsche.afd.prep <- tm_map(deutsche.afd.raw, removeNumbers) #To remove numbers

deutsche.afd <- tm_map(deutsche.afd.prep, removeWords, stopwords("english")) #use the removeWords to steam each word

deutsche.afd <- tm_map(deutsche.afd, stemDocument) #To steam the remaining words after removing the stopwords

##Checking the number of times a particular words appear in a document by using 'term frequency (tf)'
deutsche.dtm <- DocumentTermMatrix(deutsche.afd)
deutsche.dtm #to visualize non-/sparse entries, sparsity, maximal term length and weighting

# Using inspect function to take a closer look at the actual entries of this matrix
# In the last command R just showed me a summary of my deutsche.dtm

inspect(deutsche.dtm[1:363, 1:10])
deutsche.dtm.mat <- as.matrix(deutsche.dtm) #coerce the object into a standard matrix object

#using wordcloud() I can vizualise the most typed words in the articles
wordcloud(colnames(deutsche.dtm.mat), deutsche.dtm.mat[100,], max.words = 20) #to know about article 100
wordcloud(colnames(deutsche.dtm.mat), deutsche.dtm.mat[124,], max.words = 20) #to know about article 124
