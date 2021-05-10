# Webscraping
# Scrapping "The Guardian" newspaper page about Germany
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#Objective: scrape articles titles from 2018 to 2015

# Clean list
rm(list = ls())

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

#installing necessary packages
library(rvest)
library(data.table)
library(lubridate)
library(stringr)
library(datetime)


## Testing the code for 1 page

#Defining the url
url <- "https://www.theguardian.com/world/germany?page="

#Reading the url
guardian_url <- read_html(url)

#Nodes that have links
guardian_url %>%
  html_nodes("a") %>%
  head ()

guardian_url %>%
  html_nodes("a.js-headline-text") %>%
  head()

#Printing the titles without duplicates
titles <- guardian_url %>%
  html_nodes("a.js-headline-text")%>%
  html_text() %>%
  gsub("^\\s+|\\s+$", "", .)

News <- data.frame(titles)

#########

# Now looping for the 265 pages

articles <- list()

for(page in (1:265)) {
  final_url <- paste0(url,page)
  
  mydata_html <- read_html(final_url)
  all_titles <- mydata_html %>%
    html_nodes("a.js-headline-text") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$", "", .)
  
  all_urls <- mydata_html %>%
    html_nodes("a.js-headline-text") %>%
    html_attr("href")
  
my_dates <- as.character(str_extract(all_urls,"[0-9]{4}/[a-z]{3}/[0-9]{2}"),"%Y/%m/%d")

  articles[[page]] <- data.frame(all_titles = all_titles,
                                 my_dates = my_dates,
                                 all_urls = all_urls,
                                 stringsAsFactors = FALSE)
}

#concatenate all pages

final_articles <- as.data.frame(rbindlist(articles))

#transform my_dates label from character to Date

final_articles$my_dates <- anytime::anydate(final_articles$my_dates)

#Select only the time framing I need to: from 01/01/2013 to 30/11/2018

final_articles <- final_articles[final_articles$my_dates >= "2013-01-01" & final_articles$my_dates <= "2018-11-30",]
                           
#output final data
save(final_articles, file="guardian_articles.RData")
write.csv(final_articles, file="guardian_articles.csv", row.names = F)

