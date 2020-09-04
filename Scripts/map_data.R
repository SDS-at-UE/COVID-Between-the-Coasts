# map_data.R
## All map related data cleaning/collecting should go here.


# Connecting zip codes to counties
#zip_data.csv obtained through https://www.unitedstateszipcodes.org/zip-code-database/

zip_data <- read_csv("C:/Users/Darrin Weber/Desktop/zip_data.csv", 
                     col_types = cols(zip = col_character()))

zip_data <- zip_data %>% select(zip, type, primary_city, acceptable_cities, state, county, latitude, longitude)
write_csv(zip_data, "Data/zip_county.csv")


# Obtain geometry data from ACS survey for mapping purposes
# to create All_counties.shp
map_data <- get_acs(geography = "county",
                    variables = "B25077_001",
                    state = c("IN", "IL", "KY", "OH", "MI", "MN", "WI"),
                    year = 2018,
                    geometry = TRUE)

geometry_export <- select(map_data, NAME, geometry)
write_sf(geometry_export, "Data/All_counties.shp")

# Obtain geometry data from ACS survey for ZIP codes.
zip_map_data <- get_acs(geography = "zcta",
                        variables = "B25077_001",
                        year = 2018,
                        geometry = TRUE)
