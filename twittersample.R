install.packages("twitteR")
library(tidyverse)
library(twitteR)
library(openssl)
library(httpuv)
library(base64enc)
libray(readr)
setup_twitter_oauth( consumer_key="6WoyE8SKPd42xyX8cJA7PAAEo",
                     consumer_secret="vBeKwvD8AvsGlAvSUZyZSCuquuwBja7VowOg0KtNVLYGVptIsL",
                     access_token = "1327467001850322944-tv4VeJdEAnMPZ8YnhxKDWr5FDeUzyM",
                     access_secret = "4fp5a0ulTGF8Oc5EQR6rSCwkNEjzhiZS60iVHZC2O5FGg")


updateStatus(tweetfinal)

####new cases for 2020-11-01

graphic_covid <- read_csv("Data/graphic_covid.csv")
graphic_covid$date<- graphic_covid$date %>% as.character()
new_cases_for_tweet<- filter(graphic_covid, date=="2020-11-02")
new_cases_for_tweet<- new_cases_for_tweet %>% select(date, state, new_cases)
new_cases_for_tweet<- new_cases_for_tweet %>% group_by(state) %>% summarize(sum(new_cases))
new_cases_for_tweet$`sum(new_cases)`<- new_cases_for_tweet$`sum(new_cases)` %>%
                                      as.character()
dailycases<-str_c(new_cases_for_tweet$`sum(new_cases)`, sep=",")
states<- str_c(new_cases_for_tweet$state, sep=",")
tweet<- str_c( states, dailycases, sep=":")
tweetfinal<- str_c(tweet, collapse=", ")
updateStatus(tweetfinal)