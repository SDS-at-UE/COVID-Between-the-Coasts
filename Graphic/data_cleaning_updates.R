# Cleaning up the COVID data for the graphic and making it continually update

library(tidyverse)
library(stringr)
library(readr)
library(rvest)

##### Web Scraping #####

# Getting the csv files

covid_html_data <- read_html("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/") %>% 
  html_nodes('a') %>%
  html_attr('href') %>% 
  str_subset("\\.csv$")

# Extracting the data from the csv files 

cases <- read.csv(covid_html_data[1])
deaths <- read.csv(covid_html_data[2])
population <- read.csv(covid_html_data[3])

##### Data Cleaning #####

# Getting rid of unnecessary columns/rows and filtering to the 7 states we wants
cases <- cases[,-c(1,4)]
cases <- cases %>% filter(!str_detect(`County.Name`, "Statewide Unallocated"))
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

deaths <- deaths[,-c(1,4)]
deaths <- deaths %>% filter(!str_detect(`County.Name`, "Statewide Unallocated"))
deaths <- deaths %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

population <- population[,-1]
population <- population %>% filter(!str_detect(`County.Name`, "Statewide Unallocated"))
population <- population %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

# Formatting using the pivot_longer function
cases <- cases %>% 
  pivot_longer(!c(County.Name, State), names_to = "date", values_to = "cases")

deaths <- deaths %>% 
  pivot_longer(!c(County.Name, State), names_to = "date", values_to = "deaths")

# Joining the data
cases_and_deaths <- merge(cases, deaths)
cases_deaths_pop <- merge(cases_and_deaths, population)

# Making the case rate and death rate columns and renaming variables 
final_covid <- cases_deaths_pop %>% mutate(case_rate = cases/population*100000,
                                           death_rate = deaths/population*100000)

final_covid <- final_covid %>% rename(county_name = County.Name,
                                      state = State)

# Getting rid of the X in front of the date and fixing the date
final_covid$date <- str_remove(final_covid$date, "X")
final_covid$date <- gsub("\\.", "/", final_covid$date)

# writing out the final csv file 
write_csv(final_covid, "Data/graphic_covid.csv")