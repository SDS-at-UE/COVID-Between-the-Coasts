# Data Cleaning of COVID Files

library(tidyverse)

## Run/store all functions from functions.R script
source("Scripts/functions.R")






######################################
# Clean USA Facts Confirmed data
######################################

covid_conf <- read_csv("Data/usafacts_covid_confirmed.csv")

covid_conf <- covid_conf %>% select(`County Name`, State, tail(names(.),1)) %>% 
  rename_at(vars(tail(names(.),1)), funs(c("Cases")))

covid_conf <- left_join(covid_conf, state_abb_to_name, 
                        by = c("State" = "Abb")) %>% 
  select(!State) %>% 
  rename(State = State.y,
         County = `County Name`) %>% 
  mutate(NAME = str_c(County, State, sep = ", ")) %>% 
  select(NAME, Cases)

write_csv(covid_conf, "Data/covid_cases.csv")