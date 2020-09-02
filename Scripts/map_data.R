# map_data.R
## All map related data cleaning/collecting should go here.


# Connecting zip codes to counties
#zip_data.csv obtained through https://www.unitedstateszipcodes.org/zip-code-database/

zip_data <- read_csv("C:/Users/Darrin Weber/Desktop/zip_data.csv", 
                     col_types = cols(zip = col_character()))

zip_data <- zip_data %>% select(zip, type, primary_city, acceptable_cities, state, county, latitude, longitude)
write_csv(zip_data, "Data/zip_county.csv")