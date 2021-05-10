# Filtering AfD articles from the scrapped data from Deutsche Welle
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam")

#cleaning environment
rm(list = ls())

#installing necessary packages
library(lubridate)
library(datetime)
library(zoo)
library(ggplot2)
library(dplyr)
library(ggthemes)

#Reading csv file
deutsche <- read.csv("final_deutsche.csv")

#Converting Colum my_date to "Date"
deutsche$dates <- anytime::anydate(deutsche$dates)

#Creating a new row for Date with only Month/Year
deutsche$months <- substr(deutsche$dates, 0, 7)

#Converting all_months from Character to Date
deutsche$months <- as.yearmon(deutsche$months, format = "%Y-%m")
deutsche$months <- as.Date.yearmon(deutsche$months, format = "%Y-%m")

###############

##Generating dataframe for articles with quotations related to the Party

afd <- deutsche %>%
  filter(grepl("(AfD)|(German far-right party)|(Alternative für Deutschland)|(Alternative for Germany)", titles, ignore.case = TRUE))

afd$art_number <- seq.int(nrow(afd))

#output final data for AfD articles
save(afd, file="afd_deutsche_articles.RData")
write.csv(afd, file="afd_deutsche_articles.csv", row.names = F)

###############

######## Checking Frequencies ########

##Checking frequency of all articles per month

#Tabulate
deutsche_tab <- table(cut(deutsche$months, 'month'))

#Creating data frame with frequency of articles
deutsche_frequency <- data.frame(Date = format(as.Date(names(deutsche_tab)), '%m/%Y'), Frequency = as.vector(deutsche_tab))

##Checking frequency of AfD articles per month

#Tabulate
afd_tab <- table(cut(afd$months, 'month'))

#Creating data frame with frequency of articles
afd_frequency <- data.frame(Date = format(as.Date(names(afd_tab)), '%m/%Y'), Frequency = as.vector(afd_tab))

#######################

#Graph on published articles about AfD in Deutsche Welle
qplot(data = afd, x = months, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "AfD Published Articles in Deutsche Welle") + 
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90))

#Graph on published articles in Deutsche Welle
qplot(data = deutsche, x = months, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Published Articles in Deutsche Welle") + 
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90))
