# Webscraping
# Scraping the newspaper Deutsche Welle about Germany
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#Objective: Scrape the first 1000 articles

#Clean list
rm(list = ls())

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam")

#installing necessary packages
library(rvest)
library(data.table)
library(dplyr)
library(stringr)

#Defining URL
dw <- "https://www.dw.com/search/?languageCode=en&item=germany&searchNavigationId=9097&from=01.01.2013&to=30.11.2018&sort=DATE&resultsCounter=1000"

#Creating an empty list and the last.date for scraping
dfs <- list()
last.date <- "30.11.2018"

#Scraping and looping
while(as.Date(last.date, "%d.%m.%Y") > as.Date("01.01.2013","%d.%m.%Y")){
  final_url <- paste0("https://www.dw.com/search/?languageCode=en&item=germany&searchNavigationId=9097&from=01.01.2013&to=",last.date,"&sort=DATE&resultsCounter=1000")
  
  mydata_html <- read_html(final_url)
  
  titles <- mydata_html %>%
    html_nodes('.searchResult h2') %>%
    html_text(trim = T)
  
  urls = mydata_html %>%
    html_nodes(".searchResult a") %>%
    html_attr("href")
  
  dates <- mydata_html %>%
    html_nodes('.searchResult .date') %>%
    html_text(trim = T) 
  
  df <- data.frame(titles = titles,
                   dates = dates,
                   urls = urls,
                   stringsAsFactors = F)
  
  days <- as.Date(dates, "%d.%m.%Y")
  
  last.date <- format(min(days), "%d.%m.%Y")
  
  dfs[[last.date]] <- df
  
}

#concatenate all pages
final_deutsche <- as.data.frame(rbindlist(dfs))

#Clean the scraped titles from non-wanted characters
final_deutsche$titles <- str_remove(final_deutsche$titles, final_deutsche$dates)
final_deutsche$titles <- str_remove_all(final_deutsche$titles,"\\n")
final_deutsche$titles <- str_remove(final_deutsche$titles,"  ")

#Remove duplicated articles
without_duplicates <- unique(final_deutsche)
without_duplicates$dates <- as.Date(without_duplicates$dates, "%d.%m.%Y")

#Shape the final data with the desired space of time: From 01.01.2013 to 30.11.2018
without_duplicates <- without_duplicates[without_duplicates$dates >= "2013-01-01" & without_duplicates$dates <= "2018-11-30",]

#Organize URLs
without_duplicates$urls <- paste0("https://www.dw.com",without_duplicates$urls)

#Generating a new row for the number of the articles
without_duplicates$art_number <- seq.int(nrow(without_duplicates))

#output final data
save(without_duplicates, file="final_deutsche.RData")
write.csv(without_duplicates, file="final_deutsche.csv", row.names = F)