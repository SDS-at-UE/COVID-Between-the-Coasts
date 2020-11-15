#Obtaining geometry data for states and counties
library(tidyverse)
library(tidycensus)
library(sf)

## Shape file for County boundaries

map_data <- get_acs(geography = "county",
                    variables = "B25077_001",
                    state = c("IN", "IL", "KY", "OH", "MI", "MN", "WI"),
                    year = 2018,
                    geometry = TRUE)

geometry_export <- select(map_data, NAME, geometry)
write_sf(geometry_export, "Graphic/covid_between_coasts/Data/All_counties.shp")

## Shape file for State boundaries

map_data2 <- get_acs(geography = "state",
                    variables = "B25077_001",
                    state = c("IN", "IL", "KY", "OH", "MI", "MN", "WI"),
                    year = 2018,
                    geometry = TRUE)

geometry_export <- select(map_data2, NAME, geometry)
write_sf(geometry_export, "Data/All_states.shp")

