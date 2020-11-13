## This script gets the state boundary data

library(tidycensus)

map_data2 <- get_acs(geography = "state",
                    variables = "B25077_001",
                    state = c("IN", "IL", "KY", "OH", "MI", "MN", "WI"),
                    year = 2018,
                    geometry = TRUE)

geometry_export <- select(map_data2, NAME, geometry)
write_sf(geometry_export, "Data/All_states.shp")

states <- states(cb=TRUE)
states <- states %>% filter(NAME %in% c("Indiana", "Wisconsin", "Minnesota", "Illinois", "Michigan", 
                                        "Ohio", "Kentucky"))
states <- states[,-c(1:5,7:9)]
