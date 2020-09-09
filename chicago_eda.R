# Chicago EDA
## Exploring demographic data for Chicago

library(tidycensus)
library(tidyverse)
library(sf)

zip_data <- read_csv("zip_data.csv", 
                     col_types = cols(zip = col_character()))

zip_data <- zip_data %>% select(zip, type, primary_city, acceptable_cities, state, county, latitude, longitude)
write_csv(zip_data, "zip_county.csv")


# Obtain geometry data from ACS survey for mapping purposes
# to create All_counties.shp
map_data <- get_acs(geography = "county",
                    variables = "B25077_001",
                    state = c("IN", "IL", "KY", "OH", "MI", "MN", "WI"),
                    year = 2018,
                    geometry = TRUE)

geometry_export <- select(map_data, NAME, geometry)
write_sf(geometry_export, "All_counties.shp")




#******geometry = TRUE still does not run
# Obtain geometry data from ACS survey for ZIP codes.
zip_map_data <- get_acs(geography = "zcta",
                        variables = "B25077_001",
                        year = 2018,
                        geometry = TRUE)





chicago_covid <- read_csv("chicago_zip_positive_cases.csv",
                          col_types = cols(Zip = col_character()))
chicago_covid <- na.omit(chicago_covid)
chicago_covid <- chicago_covid %>% 
  mutate(case_rate = Cases/Population) %>% 
  mutate(pos_rate = Cases/Tested)

### Chicago zip codes
zip_code <- read_csv("zip_county.csv")
zip_code_chicago <- zip_code %>% filter(primary_city == "Chicago",
                                        state == "IL")


### Family Income in the Past 12 Months
chicago_income <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B19101") %>% 
                  filter(GEOID %in% zip_code_chicago$zip,
                         !variable %in% c("B19101_001"))
chicago_income <- left_join(chicago_income, variables_2018[, 1:2], by = "variable")
chicago_income$label <- as_factor(str_replace(chicago_income$label, ".*!!(.*)", "\\1"))

chicago_income_six_figure <- chicago_income %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("$200,000 or more",
                      "$150,000 to $199,999",
                      "$125,000 to $149,999",
                      "$100,000 to $124,999")) %>% 
  mutate(prop_100K = sum(prop)) %>% 
  select(GEOID, prop_100K) %>% 
  distinct(GEOID, prop_100K)

lvls <- chicago_income_six_figure %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

ggplot(chicago_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))



### Test Correlation between percentage of population making 6-figure
### incomes to the case rate of COVID

chicago_income_cor <- left_join(chicago_income_six_figure, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_income_cor$prop_100K, chicago_income_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code

chicago_income <- left_join(chicago_income, chicago_covid, by = c("GEOID" = "Zip"))


pal <- colorNumeric(palette = "viridis", domain = chicago_income$case_rate)


#****ERROR HERE
chicago_income %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(popup = str_c("<strong>", chicago_income$GEOID,
                            "</strong><br /> Case Rate ", chicago_income$case_rate),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(case_rate)) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~ case_rate,
            title = "Case Rate (per 100000)",
            opacity = 1)
#*********




#May DELETE
### Poverty Status in Past 12 Months by Age
chicago_pov <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B17020") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B17020_001"))
chicago_pov <- left_join(chicago_pov, variables_2018[, 1:2], by = "variable")
chicago_pov$label <- as_factor(str_replace(chicago_pov$label, ".*!!(.*)", "\\1"))

ggplot(chicago_pov) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))




chicago_pov <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B17020") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B17020_001"))
chicago_pov <- left_join(chicago_pov, variables_2018[, 1:2], by = "variable")
chicago_pov$label <- as_factor(str_replace(chicago_pov$label, ".*!!(.*)", "\\1"))

chicago_income_six_figure <- chicago_income %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("$200,000 or more",
                      "$150,000 to $199,999",
                      "$125,000 to $149,999",
                      "$100,000 to $124,999")) %>% 
  mutate(prop_100K = sum(prop)) %>% 
  select(GEOID, prop_100K) %>% 
  distinct(GEOID, prop_100K)

lvls <- chicago_income_six_figure %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

ggplot(chicago_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))



### Test Correlation between percentage of population making 6-figure
### incomes to the case rate of COVID

chicago_income_cor <- left_join(chicago_income_six_figure, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_income_cor$prop_100K, chicago_income_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code

chicago_income <- left_join(chicago_income, chicago_covid, by = c("GEOID" = "Zip"))


pal <- colorNumeric(palette = "viridis", domain = chicago_income$case_rate)


#****ERROR HERE
chicago_income %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(popup = str_c("<strong>", chicago_income$GEOID,
                            "</strong><br /> Case Rate ", chicago_income$case_rate),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(case_rate)) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~ case_rate,
            title = "Case Rate (per 100000)",
            opacity = 1)
#*********
#End May DELETE


### Occupation by Class of Worker for the Civilian Employed Population 16 Years and Over
chicago_occ <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "C24060") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("C24060_001"))
chicago_occ <- left_join(chicago_occ, variables_2018[, 1:2], by = "variable")
chicago_occ$label <- as_factor(str_replace(chicago_occ$label, ".*!!(.*)", "\\1"))

ggplot(chicago_occ) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))



### Sex By Age
chicago_sex <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B01001") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B01001_001"))
chicago_sex <- left_join(chicago_sex, variables_2018[, 1:2], by = "variable")
chicago_sex$label <- as_factor(str_replace(chicago_sex$label, ".*!!(.*)", "\\1"))

ggplot(chicago_sex) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Race
chicago_race <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B02001") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B02001_001"))
chicago_race <- left_join(chicago_race, variables_2018[, 1:2], by = "variable")
chicago_race$label <- as_factor(str_replace(chicago_race$label, ".*!!(.*)", "\\1"))

ggplot(chicago_race) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Nativity and Citizenship Status in the United States
chicago_citizen <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B05001") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B05001_001"))
chicago_citizen <- left_join(chicago_citizen, variables_2018[, 1:2], by = "variable")
chicago_citizen$label <- as_factor(str_replace(chicago_citizen$label, ".*!!(.*)", "\\1"))

ggplot(chicago_citizen) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### MEANS OF TRANSPORTATION TO WORK
chicago_trans <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B08301") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B08301_001"))
chicago_trans <- left_join(chicago_trans, variables_2018[, 1:2], by = "variable")
chicago_trans$label <- as_factor(str_replace(chicago_trans$label, ".*!!(.*)", "\\1"))

ggplot(chicago_trans) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))



