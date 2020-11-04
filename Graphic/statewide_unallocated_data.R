library(tidyverse)
library(stringr)
library(readr)

# Loading in the data and cleaning it 
cases <- read_csv("Data/usafacts_covid_confirmed.csv")

cases <- cases[,-c(1,4)]
cases <- cases %>% filter(str_detect(`County Name`, "Statewide Unallocated"))
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

statewide_unallocated <- cases %>% 
  pivot_longer(!c(`County Name`, State), names_to = "Date", values_to = "cases")

statewide_unallocated <- statewide_unallocated %>% rename(county_name = `County Name`)


write_csv(statewide_unallocated, "Data/graphic_covid.csv")
