# Cleaning up the COVID data for the graphic

library(tidyverse)
library(stringr)
library(readr)

# Loading in the data
cases <- read_csv("Data/usafacts_covid_confirmed.csv")
population <- read_csv("Data/usafacts_covid_county_population.csv")
deaths <- read_csv("Data/usafacts_deaths.csv")

# Getting rid of unnecessary columns/rows and filtering to the 7 states we wants
cases <- cases[,-c(1,4)]
cases <- cases %>% filter(str_detect(`County Name`, "Statewide Unallocated"))
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

deaths <- deaths[,-c(1,4)]
deaths <- deaths %>% filter(str_detect(`County Name`, "Statewide Unallocated"))
deaths <- deaths %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

population <- population[,-1]
population <- population %>% filter(str_detect(`County Name`, "Statewide Unallocated"))
population <- population %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

# Formatting using the pivot_longer function
cases <- cases %>% 
  pivot_longer(!c(`County Name`, State), names_to = "Date", values_to = "cases")

deaths <- deaths %>% 
  pivot_longer(!c(`County Name`, State), names_to = "Date", values_to = "deaths")

# Joining the data
cases_and_deaths <- merge(cases, deaths)
cases_deaths_pop <- merge(cases_and_deaths, population)

# Making the case rate and death rate columns and renaming variables 
final_covid <- cases_deaths_pop %>% mutate(case_rate = cases/population*100000,
                                           death_rate = deaths/population*100000)

final_covid <- final_covid %>% rename(statewide_unallocated = `County Name`,
                                      state = State,
                                      date = Date)

write_csv(final_covid, "Data/statewide_unallocated.csv")

