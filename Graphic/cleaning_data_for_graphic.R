# Cleaning up the COVID data for the graphic and making it continually update

library(tidyverse)
library(stringr)
library(readr)
library(rvest)
library(lubridate)
library(zoo) # for 7 day moving average

# Loading in the data
cases <- read_csv("Data/usafacts_covid_confirmed.csv")
population <- read_csv("Data/usafacts_covid_county_population.csv")
deaths <- read_csv("Data/usafacts_deaths.csv")

# Getting rid of unnecessary columns/rows and filtering to the 7 states we wants
cases <- cases[,-c(1,4)]
cases <- cases %>% filter(!str_detect(`County Name`, "Statewide Unallocated"))
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

deaths <- deaths[,-c(1,4)]
deaths <- deaths %>% filter(!str_detect(`County Name`, "Statewide Unallocated"))
deaths <- deaths %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

population <- population[,-1]
population <- population %>% filter(!str_detect(`County Name`, "Statewide Unallocated"))
population <- population %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

# Formatting using the pivot_longer function
cases <- cases %>% 
  pivot_longer(!c(`County Name`, State), names_to = "date", values_to = "cases")
cases$date <- as.Date(cases$date, "%m/%d/%y")

deaths <- deaths %>% 
  pivot_longer(!c(`County Name`, State), names_to = "date", values_to = "deaths")
deaths$date <- as.Date(deaths$date, "%m/%d/%y")

# Joining the data
cases_and_deaths <- merge(cases, deaths)
cases_deaths_pop <- merge(cases_and_deaths, population)


# Making the case rate, death rate, and 7 day moving average columns and renaming variables 
final_covid <- cases_deaths_pop %>% mutate(case_rate = cases/population*100000,
                                           death_rate = deaths/population*100000)


final_covid <- final_covid %>% rename(county_name = `County Name`,
                                      state = State)

# creating new_cases metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(new_cases = diff(c(0,cases)))

# 7 day average metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(moving_avg_7 = rollmean(new_cases, k = 7, fill = NA, align = "right"))

# writing out the final csv file 
write_csv(final_covid, "Data/graphic_covid.csv")
 
