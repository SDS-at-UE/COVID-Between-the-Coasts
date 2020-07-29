# Functions
## The following are functions used throughout the COVID/WNIN project
## This script should be run first in order to store the functions


### This function retrieves the 5-year ACS data from the Census Bureau
### for the given state and defined data/table.
get_data <- function(state, table){
  data <- get_acs(geography = "county",
                  table = table,
                  state = state,
                  year = 2018)
  data <- data %>% select(NAME, variable, estimate) %>% 
    separate(NAME, into = c("County", "State"), sep = " County,")
  return(data)
}

