# Organizing scrapped data from Deutsche Welle
# Goal: Filter the wanted articles from the original scrapped data and create dataframes from the three time-series that will be analysed
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/Deutsche Welle")

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

#output final data for AfD articles
save(afd, file="afd_deutsche_articles.RData")
write.csv(afd, file="afd_deutsche_articles.csv", row.names = F)

###############

#Formatting AfD Dates to Date format
afd$dates = as.Date(afd$dates,format='%Y-%m-%d')

##Filtering news for the first time frame for analysis (from the creation of the party to the change in its directory)
time_1 <- afd %>%
  filter(dates <= as.Date('31-12-2015',format='%d-%m-%Y'))

save(time_1, file = "time_1_deutsche.RData")
write.csv(time_1, file = "time_1_deutsche.csv", row.names = F)

#

time_2 <- afd %>%
  filter(dates > as.Date('31-12-2015', format = '%d-%m-%Y')) %>%
  filter(dates <= as.Date('30-09-2017', format = '%d-%m-%Y'))

save(time_2, file = "time_2_deutsche.RData")
write.csv(time_2, file = "time_2_deutsche.csv", row.names = F)

#

time_3 <- afd %>%
  filter(dates > as.Date('30-09-2017', format = '%d-%m-%Y'))

save(time_3, file = "time_3_deutsche.RData")
write.csv(time_3, file = "time_3_deutsche.csv", row.names = F)












########Checking Frequencies########

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

