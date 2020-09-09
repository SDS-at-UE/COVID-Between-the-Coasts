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


#################################
# Retrieve Sex, Age data
# Health insurance coverage also obtained
#################################

louis_sex_age <- get_acs(geography = "zcta",
                         table = "B27001") %>% 
  filter(GEOID %in% zip_code_louis$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable")

louis_sex_age <- louis_sex_age %>% filter(str_count(label, "!!") >= 4)

louis_sex_age$label <- str_remove(louis_sex_age$label, "Estimate!!Total!!")

louis_sex_age <- separate(louis_sex_age,
                          label,
                          sep = "!!",
                          into = c("Sex", "Age", "HI_Coverage"))

louis_sex_age$HI_Coverage <- if_else(louis_sex_age$HI_Coverage == "No health insurance coverage", "No", "Yes")

louis_sex_age$Sex <- as_factor(louis_sex_age$Sex)
louis_sex_age$Age <- as_factor(louis_sex_age$Age)
louis_sex_age$HI_Coverage <- as_factor(louis_sex_age$HI_Coverage)  

## Look at Health Insurance Coverage

louis_HI <- louis_sex_age %>% group_by(GEOID, HI_Coverage) %>% 
  summarize(count = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  mutate(pop = sum(count),
         prop = count/pop)

### Graph Health Insurance Coverage by proportion

louis_HI_levels <- louis_HI %>% 
  filter(HI_Coverage == "Yes") %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(louis_HI) +
  geom_col(aes(factor(GEOID, levels = louis_HI_levels), count, fill = HI_Coverage),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Scatterplot HI_Coverage vs case rate

louis_HI_cor <- left_join(louis_HI, louis_covid, by = c("GEOID"="zip"))

filter(louis_HI_cor, HI_Coverage == "Yes") %>%
  ggplot() +
  geom_point(aes(prop, case_rate))

#### Remove 40202 as outlier and conduct correlation test

louis_HI_cor_sans40202 <- louis_HI_cor %>% 
  filter(HI_Coverage == "Yes",
         case_rate < .03)
cor.test(louis_HI_cor_sans40202$prop, louis_HI_cor_sans40202$case_rate, use = "complete.obs")

## Look at Ages of the zip codes

louis_age <- louis_sex_age %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(Age %in% c("65 to 74 years",
                    "75 years and over")) %>% 
  mutate(prop_over65 = sum(prop)) %>% 
  arrange(prop_over65)

age_lvls <- louis_sex_age %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(Age %in% c("65 to 74 years",
                    "75 years and over")) %>% 
  mutate(prop_over65 = sum(prop)) %>% 
  arrange(prop_over65) %>% 
  select(GEOID, prop_over65) %>% 
  distinct(GEOID< prop_over65) %>% 
  pull(GEOID)

ggplot(louis_sex_age) +
  geom_col(aes(factor(GEOID, levels = age_lvls), estimate, fill = Age),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Correlation test between proportion over 65 and case rate

louis_age_cor <- left_join(louis_age, louis_covid, by = c("GEOID"="zip"))

ggplot(louis_age_cor) +
  geom_point(aes(prop_over65, case_rate))

#### Take out 40202 outlier

louis_age_cor_sans40202 <- filter(louis_age_cor, case_rate < .03)

cor.test(louis_age_cor_sans40202$prop_over65, louis_age_cor_sans40202$case_rate, use = "complete.obs")


##################################
# Private vs. Public HI
##################################

louis_hi_private <- get_acs(geography = "zcta",
                            table = "B27002") %>% 
  filter(GEOID %in% zip_code_louis$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable") %>% 
  filter(str_count(label, "!!") >= 4)

louis_hi_public <- get_acs(geography = "zcta",
                           table = "B27003") %>% 
  filter(GEOID %in% zip_code_louis$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable") %>% 
  filter(str_count(label, "!!") >= 4)

louis_hi_private$label <- str_remove(louis_hi_private$label, "Estimate!!Total!!")
louis_hi_public$label <- str_remove(louis_hi_public$label, "Estimate!!Total!!")

louis_hi_private <- separate(louis_hi_private,
                             label,
                             sep = "!!",
                             into = c("Sex", "Age", "Private_HI"))
louis_hi_public <- separate(louis_hi_public,
                            label,
                            sep = "!!",
                            into = c("Sex", "Age", "Public_HI"))

louis_hi_private$Private_HI <- if_else(louis_hi_private$Private_HI == "No private health insurance", "No", "Yes")
louis_hi_public$Public_HI <- if_else(louis_hi_public$Public_HI == "No public coverage", "No", "Yes")

louis_hi_private$Sex <- as_factor(louis_hi_private$Sex)
louis_hi_public$Sex <- as_factor(louis_hi_public$Sex)
louis_hi_private$Age <- as_factor(louis_hi_private$Age)
louis_hi_public$Age <- as_factor(louis_hi_public$Age)
louis_hi_private$Private_HI <- as_factor(louis_hi_private$Private_HI)
louis_hi_public$Public_HI <- as_factor(louis_hi_public$Public_HI) 

## Look at Private HI Coverage

louis_hi_private_2 <- louis_hi_private %>% group_by(GEOID, Private_HI) %>% 
  summarize(count = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  mutate(pop = sum(count),
         prop = count/pop)

### Graph Private HI Coverage by proportion

lvls_louis_hi_private <- louis_hi_private_2 %>% 
  filter(Private_HI == "Yes") %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(louis_hi_private_2) +
  geom_col(aes(factor(GEOID, levels = lvls_louis_hi_private), count, fill = Private_HI),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Scatterplot Private HI vs case rate

louis_hi_private_cor <- left_join(louis_hi_private_2, louis_covid, by = c("GEOID"="zip"))

filter(louis_hi_private_cor, Private_HI == "Yes") %>%
  ggplot(aes(prop, case_rate)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = GEOID)) +
  labs(title = "Private HI vs Case Rate")

#### Remove 40202 as outlier and conduct correlation test

louis_hi_private_cor_sans40202 <- louis_hi_private_cor %>% 
  filter(Private_HI == "Yes",
         case_rate < .03)
cor.test(louis_hi_private_cor_sans40202$prop, louis_hi_private_cor_sans40202$case_rate, use = "complete.obs")


## Look at Public HI Coverage

louis_hi_public_2 <- louis_hi_public %>% group_by(GEOID, Public_HI) %>% 
  summarize(count = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  mutate(pop = sum(count),
         prop = count/pop)

### Graph Public HI Coverage by proportion

lvls_louis_hi_public <- louis_hi_public_2 %>% 
  filter(Public_HI == "Yes") %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(louis_hi_public_2) +
  geom_col(aes(factor(GEOID, levels = lvls_louis_hi_public), count, fill = Public_HI),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#### Order the x-axis based on proportion of Private HI to see how it matches up
#### with the public HI option.
ggplot(louis_hi_public_2) +
  geom_col(aes(factor(GEOID, levels = lvls_louis_hi_private), count, fill = Public_HI),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Proportion of Population with Public Health Insurance",
       subtitle = "Ordered by Increasing Proportion of Population with Private Health Insurance")

### Scatterplot Public HI vs case rate

louis_hi_public_cor <- left_join(louis_hi_public_2, louis_covid, by = c("GEOID"="zip"))

filter(louis_hi_public_cor, Public_HI == "Yes") %>%
  ggplot(aes(prop, case_rate)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = GEOID)) +
  labs(title = "Public HI vs Case Rate")

#### Remove 40202 as outlier and conduct correlation test

louis_hi_public_cor_sans40202 <- louis_hi_public_cor %>% 
  filter(Public_HI == "Yes",
         case_rate < .03)
cor.test(louis_hi_public_cor_sans40202$prop, louis_hi_public_cor_sans40202$case_rate, use = "complete.obs")

## In looking at the private HI vs. case rate scatterplot, there are some definitive
## groups. What is causing that?






