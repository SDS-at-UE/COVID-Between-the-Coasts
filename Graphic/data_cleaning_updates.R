# Cleaning up the COVID data for the graphic and making it continually update

library(tidyverse)
library(stringr)
library(readr)
library(rvest)
library(lubridate)
library(zoo) # for 7 day moving average

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

# removing the X in front of the date, making the dots / in the dates, and fixing the date
# had to do this to each cases and deaths before the merge so that the dates would merge correctly
cases$date <- str_remove(cases$date, "X")
cases$date <- gsub("\\.", "/", cases$date)
cases$date <- as.Date(cases$date, "%m/%d/%y")

deaths$date <- str_remove(deaths$date, "X")
deaths$date <- gsub("\\.", "/", deaths$date)
deaths$date <- as.Date(deaths$date, "%m/%d/%y")

# Joining the data
cases_and_deaths <- merge(cases, deaths)
cases_deaths_pop <- merge(cases_and_deaths, population)

# Making the case rate and death rate columns and renaming variables 
final_covid <- cases_deaths_pop %>% mutate(case_rate = cases/population*100000,
                                           death_rate = deaths/population*100000)

final_covid <- final_covid %>% rename(county_name = County.Name,
                                      state = State)

# creating new_cases metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(new_cases = diff(c(0,cases)))

# creating 7 day moving average metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(moving_avg_7 = rollmean(new_cases, k = 7, fill = NA, align = "right"))

# writing out the final csv file 
write_csv(final_covid, "Data/graphic_covid.csv")

