# louisville_eda.R
## EDA on zip code data comparisons for Louisville, KY.
## Goal is to identify zip codes with differing characteristics
## and compare their COVID numbers

source("Scripts/functions.R")

######################################
# Retrieve zip code to county dataset
######################################

zip_code <- read_csv("Data/zip_county.csv")
zip_code_louis <- zip_code %>% filter(primary_city == "Louisville",
                                      state == "KY")

######################################
# Retrieve COVID case data for Louisville
# collected from https://covid-19-in-jefferson-county-ky-lojic.hub.arcgis.com/
#####################################

louis_covid <- read_csv("Data/louisville_covid.csv",
                        col_types = cols(ZIP = col_character()))
louis_covid <- louis_covid %>% 
  mutate(case_rate = Cases/Population) %>% 
  rename(zip = ZIP,
         population = Population,
         cases = Cases)

######################################
# Retrieve Income data
######################################

louis_income <- get_acs(geography = "zcta",
                        table = "B19101",
                        geometry = TRUE) %>% 
  filter(GEOID %in% zip_code_louis$zip,
         !variable %in% c("B19101_001"))

louis_income <- left_join(louis_income, variables_2018[, 1:2], by = "variable")

louis_income$label <- as_factor(str_replace(louis_income$label, ".*!!(.*)", "\\1"))

# Graph income data to see differences in zip codes

## Order the x-axis of the graph based on income. 
### We group 6-figure incomes together and look
### at their proportion of the zip code and 
### sort in ascending order.
louis_income_six_figure <- louis_income %>% 
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

lvls <- louis_income_six_figure %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

ggplot(louis_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population making 6-figure
### incomes to the case rate of COVID

louis_income_cor <- left_join(louis_income_six_figure, louis_covid, by = c("GEOID"="zip"))
cor.test(louis_income_cor$prop_100K, louis_income_cor$case_rate, use = "complete.obs")

### Scatterplot of six-figure income vs. case rate

ggplot(louis_income_cor) +
  geom_point(aes(prop_100K, case_rate))

### Determine outlier from above scatterplot

filter(louis_income_cor, case_rate > .025)

#### ZIP 40202 is an outlier. Remove and retest correlation

louis_income_cor_sans40202 <- louis_income_cor %>% 
  filter(case_rate < .025)

cor.test(louis_income_cor_sans40202$prop_100K, louis_income_cor_sans40202$case_rate, use = "complete.obs")

ggplot(louis_income_cor_sans40202) +
  geom_point(aes(prop_100K, case_rate))

################################
# Plot COVID data for zip code
################################

louis_income <- left_join(louis_income, louis_covid, by = c("GEOID" = "zip"))


pal <- colorNumeric(palette = "viridis", domain = louis_income$case_rate)

louis_income %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(popup = str_c("<strong>", louis_income$GEOID,
                            "</strong><br /> Case Rate ", louis_income$case_rate),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(case_rate)) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~ case_rate,
            title = "Case Rate (per 100000)",
            opacity = 1)

#################################
# Retrieve Gini Index
#################################

louis_gini <- get_acs(geography = "zcta",
                      table = "B19083",
                      geometry = TRUE)

# Export/Write ZIP code geometry data for use in other scripts

geometry_zip_export <- select(louis_gini, GEOID, geometry)
write_sf(geometry_zip_export, "Data/All_zips.shp")

# Back to Gini Index

louis_gini <- louis_gini %>% 
  filter(GEOID %in% zip_code_louis$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable")

louis_gini$label <- as_factor(str_replace(louis_gini$label, ".*!!(.*)", "\\1"))

# Map Gini Index

pal_gini <- colorNumeric(palette = "viridis", domain = louis_gini$estimate)

louis_gini %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(popup = str_c("<strong>", louis_gini$GEOID,
                            "</strong><br /> Gini Index: ", louis_gini$estimate),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal_gini(estimate)) %>% 
  addLegend("bottomright",
            pal = pal_gini,
            values = ~ estimate,
            title = "Gini Index",
            opacity = 1)

### Test Correlation between Gini index and the case rate of COVID

louis_gini_cor <- left_join(louis_gini, louis_covid, by = c("GEOID"="zip"))
cor.test(louis_gini_cor$estimate, louis_gini_cor$case_rate, use = "complete.obs")

### Scatterplot Gini Index and Case rate

ggplot(louis_gini_cor) +
  geom_point(aes(estimate, case_rate))

### Remove outlier and retest correlation for gini to case rate

filter(louis_gini_cor, case_rate > .03)

louis_gini_cor_sans40202 <- filter(louis_gini_cor, case_rate < .03)

cor.test(louis_gini_cor_sans40202$estimate, louis_gini_cor_sans40202$case_rate, use = "complete.obs")
