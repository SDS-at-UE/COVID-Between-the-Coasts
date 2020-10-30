library(tidyverse)
library(sf)
library(tigris)

## states_map gives NAME in format of "Vanderburgh County, Indiana"
states_map <- read_sf("Data/All_counties.shp", type = 6)

#graphic_covid gices county_name as "Vanderburgh County" and a separate state column with "IN"
graphic_covid <- read_csv("Data/graphic_covid.csv")

state_abb_to_name <- tibble(State = state.name, Abb = state.abb)

#Making new column for name of states
covid_data <- left_join(graphic_covid, state_abb_to_name, by = c("state"= "Abb"))

#Combine county_name and new state column with a comma between them to match format of states_map
#Use paste?
covid_data <- covid_data %>% mutate(NAME = str_c(county_name, State, sep = ', '))


#Joining two datasets
covid_map_data <- geo_join(states_map, covid_data, by = "NAME")

