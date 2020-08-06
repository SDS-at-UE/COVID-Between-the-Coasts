# Data Cleaning of COVID Files

library(tidyverse)

## Run/store all functions from functions.R script
source("Scripts/functions.R")



######################################
# Clean USA Facts data
######################################

### #####################
### Clean Confirmed Cases data
### #####################

covid_conf <- read_csv("Data/usafacts_covid_confirmed.csv")

covid_conf <- covid_conf %>% select(`County Name`, State, tail(names(.),1)) %>% 
  rename_at(vars(tail(names(.),1)), funs(c("Cases"))) %>% 
  filter(State %in% c("IN", "IL", "KY", "OH", "MI", "MN", "WI"))

covid_conf <- left_join(covid_conf, state_abb_to_name, 
                        by = c("State" = "Abb")) %>% 
  rename(State_abb = State,
         State = State.y,
         County = `County Name`) %>% 
  mutate(NAME = str_c(County, State, sep = ", "))


### #####################
### Clean Population data
### #####################

county_pop <- read_csv("Data/usafacts_covid_county_population.csv")

county_pop <- county_pop %>% select(`County Name`, State, tail(names(.),1)) %>% 
  rename_at(vars(tail(names(.),1)), funs(c("Population"))) %>% 
  filter(State %in% c("IN", "IL", "KY", "OH", "MI", "MN", "WI"))

county_pop <- left_join(county_pop, state_abb_to_name, 
                        by = c("State" = "Abb")) %>%
  rename(State_abb = State,
         State = State.y,
         County = `County Name`) %>% 
  mutate(NAME = str_c(County, State, sep = ", "))

### #####################
### Clean Deaths data
### #####################

covid_death <- read_csv("Data/usafacts_deaths.csv")

covid_death <- covid_death %>% select(`County Name`, State, tail(names(.),1)) %>% 
  rename_at(vars(tail(names(.),1)), funs(c("Deaths"))) %>% 
  filter(State %in% c("IN", "IL", "KY", "OH", "MI", "MN", "WI"))

covid_death <- left_join(covid_death, state_abb_to_name, 
                         by = c("State" = "Abb")) %>%
  rename(State_abb = State,
         State = State.y,
         County = `County Name`) %>% 
  mutate(NAME = str_c(County, State, sep = ", "))

### #####################
### Combine Datasets to create master USA Facts data
### #####################

usafacts_covid <- left_join(covid_conf, covid_death, by = c("NAME", "County", "State", "State_abb")) %>% 
  left_join(county_pop, by = c("NAME", "County", "State", "State_abb")) %>%
  select(NAME, County, State, State_abb, everything())

usafacts_covid <- usafacts_covid %>% 
  mutate(case_fatality = round(Deaths/na_if(Cases, 0)*100, 3),
         death_rate = round(Deaths/na_if(Population, 0)*100000, 3),
         case_rate = round(Cases/na_if(Population, 0)*100000, 3)) 

write_csv(usafacts_covid, "Data/covid_data.csv")



