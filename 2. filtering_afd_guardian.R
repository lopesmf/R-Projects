# Filtering articles about AfD
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

#cleaning environment
rm(list = ls())

#installing necessary packages
library(lubridate)
library(datetime)
library(zoo)
library(ggplot2)
library(dplyr)

#Reading csv file
guardian <- read.csv("guardian_articles.csv")

#Converting Colum my_date to "Date"
guardian$my_dates <- anytime::anydate(guardian$my_dates)

#Creating a new row for Date with only Month/Year
guardian$all_months <- substr(guardian$my_dates, 0, 7)

#Converting all_months from Character to Date
guardian$all_months <- as.yearmon(guardian$all_months, format = "%Y-%m")
guardian$all_months <- as.Date.yearmon(guardian$all_months, format = "%Y-%m")


###############

##Generating dataframe for articles with quotations related to the Party

afd <- guardian %>%
  filter(grepl("(AfD)|(German far-right party)|(Alternative für Deutschland)|(Alternative for Germany)", all_titles, ignore.case = TRUE))

#output final data for AfD articles
save(afd, file="afd_guardian_articles.RData")
write.csv(afd, file="afd_guardian_articles.csv", row.names = F)

###############

########Checking Frequencies########

##Checking frequency of all articles per month

#Tabulate
guardian_tab <- table(cut(guardian$all_months, 'month'))

#Creating data frame with frequency of articles
guardian_frequency <- data.frame(Date = format(as.Date(names(guardian_tab)), '%m/%Y'), Frequency = as.vector(guardian_tab))

##Checking frequency of AfD articles per month

#Tabulate
afd_tab <- table(cut(guardian$all_months, 'month'))

#Creating data frame with frequency of articles
afd_frequency <- data.frame(Date = format(as.Date(names(afd_tab)), '%m/%Y'), Frequency = as.vector(afd_tab))

#######################

#Graph on published articles about AfD in The Guardian
qplot(data = afd, x = all_months, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "AfD Published Articles in The Guardian") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#Graph on published articles in The Guardian
qplot(data = guardian, x = all_months, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Published Articles in The Guardian") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
