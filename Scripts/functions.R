# Functions
## The following are functions used throughout the COVID/WNIN project
## This script should be run first in order to store the functions

library(tidycensus)
library(tidyverse)
library(leaflet)
library(stringr)
library(sf)
library(tigris)

census_api_key("7cf0c318e343f70900ce428bc2646b7f776807e5",
              install = TRUE)

### This loads the necessary variable-to-table data to connect table
### numbers in the Census dataset to their actual meanings. It should
### only need to run once on a computer.
variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>%
  rename(variable = name)


### This function retrieves the 5-year ACS data from the Census Bureau
### for the given state and defined data/table.
get_data <- function(state, table, region = "county"){
  data <- get_acs(geography = region,
                  table = table,
                  state = state,
                  year = 2018)
  data <- data %>% select(NAME, variable, estimate) %>% 
    separate(NAME, into = c("County", "State"), sep = " County,", remove = FALSE)
  data <- left_join(data, variables_2018[, 1:2], by = "variable")
  return(data)
}


### This creates a reference database to convert between the 
### name of a state and its abbreviation
state_abb_to_name <- tibble(State = state.name, Abb = state.abb)

