# Collecting full text of AfD articles
# Generating files for the three phases of the party
# Maria Fernanda Lopes Ferreira Del Ducca
# au516257  201400917

#cleaning environment
rm(list = ls())

#set as working directory
setwd("~/Documents/Masters'/2.2018/Political Data Science/Final Exam/The Guardian")

#installing necessary packages
library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#create directories that do not exist to keep the txt files
if(!dir.exists("guardian_txt"))dir.create("guardian_txt")
if(!dir.exists("guardian_phase_1"))dir.create("guardian_phase_1")
if(!dir.exists("guardian_phase_2"))dir.create("guardian_phase_2")
if(!dir.exists("guardian_phase_3"))dir.create("guardian_phase_3")
if(!dir.exists("guardian_phase_4"))dir.create("guardian_phase_4")

#Reading csv file
afd <- read.csv("afd_guardian_articles.csv")

#Collecting full text from articles (using gsub to clean the text from non-wanted objects, such as links to twitter, facebook or even to other articles)

full_text <- rep("",nrow(afd))
for(i in 1:nrow(afd)){
  try({
    full_text[i] <- read_html(as.character(afd$all_urls[i])) %>%
      html_nodes("div.content__article-body.from-content-api.js-article__body") %>% 
      html_text(trim = T) %>%
      gsub("\\n\\n\\n\\n(.*)Read more\\n\\n\\n\\n","", .) %>% 
      gsub("Topics(.*)Reuse this content","", .) %>%
      gsub("Facebook(.*)Twitter(.*)Pinterest(.*)","", .) %>%
      gsub("Was this helpful?(.*)Thank you for your feedback(.*)", "", .) 
  })
}


#Creating new row with full text
afd$full_text <- full_text

afd$art_number <- seq.int(nrow(afd))

#output final data for AfD articles with full text
save(afd, file="afd_guardian_articles_full_text.RData")
write.csv(afd, file="afd_guardian_articles_full_text.csv", row.names = F)

#### 

#Removing rows without text in the column full_text
afd <- afd %>%
  filter(grepl(" ", full_text, ignore.case = TRUE))

afd$art_number <- seq.int(nrow(afd))

# Saving as txt
for(i in 1:nrow(afd)){
  writeLines(afd$full_text[i],con=paste0('guardian_txt/art',i,'.txt'))
}

# Formatting dates: Transforming factor to Date
afd$dates <- as.Date(afd$my_dates, format = "%Y-%m-%d")

#Selecting the phases in the timeline to do the analysis by period
phase_1 <- afd[afd$my_dates >="2013-01-01" & afd$my_dates <= "2014-05-31",]
phase_2 <- afd[afd$my_dates >="2014-06-01" & afd$my_dates <= "2015-07-30",]
phase_3 <- afd[afd$my_dates >="2015-08-01" & afd$my_dates <= "2017-09-30",]
phase_4 <- afd[afd$my_dates >="2017-10-01",]

for (i in 1:nrow(afd)){
  if(afd$my_dates[i]>="2013-01-01" & afd$my_dates[i] <= "2014-05-31"){
    afd$Phases[i] = 1
  } else if (afd$my_dates[i]>="2014-06-01" & afd$my_dates[i] <= "2015-07-30"){
    afd$Phases[i] = 2
  } else if (afd$my_dates[i] >="2015-08-01" & afd$my_dates[i] <= "2017-09-30"){
    afd$Phases[i] = 3
  } else if (afd$my_dates[i]>="2017-10-01"){
    afd$Phases[i] = 4}
}

#output final data for AfD articles with full text and Phases added
save(afd, file="afd_guardian_articles_full_text.RData")
write.csv(afd, file="afd_guardian_articles_full_text.csv", row.names = F)

#####Spliting the content into the four phases of the party and saving it separetly

#Formatting AfD Dates to Date format
afd$my_dates <- as.Date(afd$my_dates,format='%Y-%m-%d')

#Phase 1
phase_1 <- afd[afd$my_dates >="2013-01-01" & afd$my_dates <= "2014-05-31",]

save(phase_1, file = "guardian_phase_1/phase_1_guardian.RData") #saving in R Data format
write.csv(phase_1, file = "guardian_phase_1/phase_1_guardian.csv", row.names = F) #saving in csv format

for(i in 1:nrow(phase_1)){
  writeLines(phase_1$full_text[i],con=paste0('guardian_phase_1/art',i,".txt"))
} #saving in txt files

#Phase 2
phase_2 <- afd[afd$my_dates >="2014-06-01" & afd$my_dates <= "2015-07-30",]

save(phase_2, file = "guardian_phase_2/phase_2_guardian.RData") #saving in R Data format
write.csv(phase_2, file = "guardian_phase_2/phase_2_guardian.csv", row.names = F) #saving in csv format

for(i in 1:nrow(phase_2)){
  writeLines(phase_2$full_text[i],con=paste0('guardian_phase_2/art',i,'.txt'))
} #saving in txt files


#Phase 3
phase_3 <- afd[afd$my_dates >="2015-08-01" & afd$my_dates <= "2017-09-30",]

save(phase_3, file = "guardian_phase_3/phase_3_guardian.RData") #saving in R Data format
write.csv(phase_3, file = "guardian_phase_3/phase_3_guardian.csv", row.names = F) #saving in csv format

for(i in 1:nrow(phase_3)){
  writeLines(phase_3$full_text[i],con=paste0('guardian_phase_3/art',i,'.txt'))
} #saving in txt files

#Phase 4
phase_4 <- afd[afd$my_dates >="2017-10-01",]

save(phase_4, file = "guardian_phase_4/phase_4_guardian.RData") #saving in R Data format
write.csv(phase_4, file = "guardian_phase_4/phase_4_guardian.csv", row.names = F) #saving in csv format

for(i in 1:nrow(phase_4)){
  writeLines(phase_4$full_text[i],con=paste0('guardian_phase_4/art',i,".txt"))
} #saving in txt files


##############


# Checking Frequencies of the articles

#Converting all_months from Character to Date
afd$all_months <- as.yearmon(afd$all_months, format = "%Y-%m")
afd$all_months <- as.Date.yearmon(afd$all_months, format = "%Y-%m")

#Tabulate
guardian_tab <- table(cut(afd$all_months, 'month'))

#Creating data frame with frequency of articles
guardian_frequency <- data.frame(Date = format(as.Date(names(guardian_tab)), '%m/%Y'), Frequency = as.vector(guardian_tab))

##Checking frequency of AfD articles per month

#Tabulate
afd_tab <- table(cut(afd$all_months, 'month'))

#Creating data frame with frequency of articles
afd_frequency <- data.frame(Date = format(as.Date(names(afd_tab)), '%m/%Y'), Frequency = as.vector(afd_tab))

#######################

#Graph on published articles about AfD in The Guardian
qplot(data = afd, x = all_months, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "AfD Published Articles in The Guardian") + 
  theme_minimal() + geom_bar(stat = "count", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 0))

#Graph on published articles in The Guardian
qplot(data = guardian, x = all_months, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Published Articles in The Guardian") + 
  theme_minimal() + geom_bar(stat = "count", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 0))
