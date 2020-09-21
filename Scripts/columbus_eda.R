# columbus_eda.R
## EDA on zip code data comparisons for Columbus, OH.
## Goal is to identify zip codes with differing characteristics
## and compare their COVID numbers

source("Scripts/functions.R")
library(readxl)

######################################
# Retrieve zip code to county dataset
######################################

zip_code <- read_csv("Data/zip_county.csv")
zip_code_col <- zip_code %>% filter(primary_city == "Columbus", state == "OH")
                                      
# This has 47 zip codes, on the excel file we have 51. 

######################################
# Retrieve ZIP geometry data
######################################

geometry_zip <- read_sf("Data/All_zips.shp", type = 6)
geometry_zip_col <- filter(geometry_zip, GEOID %in% zip_code_col$zip)

# The geometry zip only has 30 observations while zip_code_col has 47.

######################################
# Retrieve COVID case data for Columbus
# collected on 9/17
#####################################

col_covid <- read_excel("Data/columbus_covid.xlsx")

######################################
# Retrieve Income data
######################################

col_income <- get_acs(geography = "zcta",
                        table = "B19101") %>% 
  filter(GEOID %in% zip_code_col$zip,
         !variable %in% c("B19101_001"))

col_income <- left_join(col_income, variables_2018[, 1:2], by = "variable")

col_income$label <- as_factor(str_replace(col_income$label, ".*!!(.*)", "\\1"))

col_income <- left_join(col_income, geometry_zip_col, by = "GEOID")

col_income <- st_as_sf(col_income, sf_column_name = "geometry")

# Graph income data to see differences in zip codes

## Order the x-axis of the graph based on income. 
### We group 6-figure incomes together and look
### at their proportion of the zip code and 
### sort in ascending order.
col_income_six_figure <- col_income %>% 
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

lvls <- col_income_six_figure %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

ggplot(col_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# ANOVA TEST
col_covid$ZIP <- as.character(col_covid$ZIP)
col_income_six_figure <- left_join(col_income_six_figure, col_covid, 
                                   by = c("GEOID" = "ZIP"))

income_lm <- lm(average ~ prop_100K, data = col_income_six_figure)
summary(income_lm)
anova(income_lm)
#When making a linear model of average cases per ZIP and the proportion of 
#those who make 6 figures, we see both the intercept and coefficent of the model
# are significant.
#After performing the anova, the proportion of those who make 6 figures is also 
# significant with a p-value  of 0.005373

#################################
# Retrieve Gini Index
#################################

col_gini <- get_acs(geography = "zcta",
                      table = "B19083")

col_gini <- left_join(col_gini, geometry_zip_col, by = "GEOID")

col_gini <- st_as_sf(col_gini, sf_column_name = "geometry")

col_gini <- col_gini %>% 
  filter(GEOID %in% zip_code_col$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable")

col_gini$label <- as_factor(str_replace(col_gini$label, ".*!!(.*)", "\\1"))

# Map Gini Index

pal_gini <- colorNumeric(palette = "viridis", domain = col_gini$estimate)

col_gini %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(popup = str_c("<strong>", col_gini$GEOID,
                            "</strong><br /> Gini Index: ", col_gini$estimate),
              weight = 2,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal_gini(estimate)) %>% 
  addLegend("bottomright",
            pal = pal_gini,
            values = ~ estimate,
            title = "Gini Index",
            opacity = 1)

## ANOVA TEST
col_gini <- left_join(col_gini, col_covid, by = c("GEOID" = "ZIP"))

gini_lm <- lm(average ~ estimate, data = col_gini)
summary(gini_lm)
anova(gini_lm)
#Our linear model and anova test give no significance of values with p-value of 0.1586.

#################################
# Retrieve Sex, Age data
# Health insurance coverage also obtained
#################################

col_sex_age <- get_acs(geography = "zcta",
                         table = "B27001") %>% 
  filter(GEOID %in% zip_code_col$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable")

col_sex_age <- col_sex_age %>% filter(str_count(label, "!!") >= 4)

col_sex_age$label <- str_remove(col_sex_age$label, "Estimate!!Total!!")

col_sex_age <- separate(col_sex_age,
                          label,
                          sep = "!!",
                          into = c("Sex", "Age", "HI_Coverage"))

col_sex_age$HI_Coverage <- if_else(
  col_sex_age$HI_Coverage == "No health insurance coverage", "No", "Yes")

col_sex_age$Sex <- as_factor(col_sex_age$Sex)
col_sex_age$Age <- as_factor(col_sex_age$Age)
col_sex_age$HI_Coverage <- as_factor(col_sex_age$HI_Coverage)  

## Look at Health Insurance Coverage

col_HI <- col_sex_age %>% group_by(GEOID, HI_Coverage) %>% 
  summarize(count = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  mutate(pop = sum(count),
         prop = count/pop)

### Graph Health Insurance Coverage by proportion

lvls_col_HI <- col_HI %>% 
  filter(HI_Coverage == "Yes") %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(col_HI) +
  geom_col(aes(factor(GEOID, levels = lvls_col_HI), count, fill = HI_Coverage),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## Look at Ages of the zip codes

col_age <- col_sex_age %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(Age %in% c("65 to 74 years",
                    "75 years and over")) %>% 
  mutate(prop_over65 = sum(prop)) %>% 
  arrange(prop_over65)

lvls_age <- col_sex_age %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(Age %in% c("65 to 74 years",
                    "75 years and over")) %>% 
  mutate(prop_over65 = sum(prop)) %>% 
  arrange(prop_over65) %>% 
  select(GEOID, prop_over65) %>% 
  distinct(GEOID, prop_over65) %>% 
  pull(GEOID)

ggplot(col_sex_age) +
  geom_col(aes(factor(GEOID, levels = lvls_age), estimate, fill = Age),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## ANOVA Test 
col_age <- left_join(col_age, col_covid, by = c("GEOID" = "ZIP"))

age_lm <- lm(average ~ prop_over65, data = col_age)
summary(age_lm)
anova(age_lm)
#A very small, significant p-value is associated with this analysis of individuals over 65 and average number of
#COVID-19 cases per ZIP code

##################################
# Private vs. Public HI
##################################

col_hi_private <- get_acs(geography = "zcta",
                            table = "B27002") %>% 
  filter(GEOID %in% zip_code_col$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable") %>% 
  filter(str_count(label, "!!") >= 4)

col_hi_public <- get_acs(geography = "zcta",
                           table = "B27003") %>% 
  filter(GEOID %in% zip_code_col$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable") %>% 
  filter(str_count(label, "!!") >= 4)

col_hi_private$label <- str_remove(col_hi_private$label, "Estimate!!Total!!")
col_hi_public$label <- str_remove(col_hi_public$label, "Estimate!!Total!!")

col_hi_private <- separate(col_hi_private,
                             label,
                             sep = "!!",
                             into = c("Sex", "Age", "Private_HI"))
col_hi_public <- separate(col_hi_public,
                            label,
                            sep = "!!",
                            into = c("Sex", "Age", "Public_HI"))

col_hi_private$Private_HI <- if_else(
  col_hi_private$Private_HI == "No private health insurance", "No", "Yes")
col_hi_public$Public_HI <- if_else(
  col_hi_public$Public_HI == "No public coverage", "No", "Yes")

col_hi_private$Sex <- as_factor(col_hi_private$Sex)
col_hi_public$Sex <- as_factor(col_hi_public$Sex)
col_hi_private$Age <- as_factor(col_hi_private$Age)
col_hi_public$Age <- as_factor(col_hi_public$Age)
col_hi_private$Private_HI <- as_factor(col_hi_private$Private_HI)
col_hi_public$Public_HI <- as_factor(col_hi_public$Public_HI) 

## Look at Private HI Coverage

col_hi_private_2 <- col_hi_private %>% group_by(GEOID, Private_HI) %>% 
  summarize(count = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  mutate(pop = sum(count),
         prop = count/pop)

### Graph Private HI Coverage by proportion

lvls_col_hi_private <- col_hi_private_2 %>% 
  filter(Private_HI == "Yes") %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(col_hi_private_2) +
  geom_col(aes(factor(GEOID, levels = lvls_col_hi_private), count, fill = Private_HI),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## ANOVA TEST
col_hi_private_2 <- col_hi_private_2 %>% filter(Private_HI == "Yes")
col_hi_private_2 <- left_join(col_hi_private_2, col_covid, by = c("GEOID" = "ZIP"))

hi_lm <- lm(average ~ prop, data = col_hi_private_2)
summary(hi_lm)
anova(hi_lm)
#Our anova test suggests there is nothing significant between private health insurance
#and COVID-19 cases. The p-value is 0.1913


################################
# Occupation (essential vs. non-essential)
################################

col_occ <- get_acs(geography = "zcta",
                     table = "C24060") %>% 
  filter(GEOID %in% zip_code_col$zip,
         variable %in% str_c("C24060_00", 2:6))
col_occ <- left_join(col_occ, variables_2018[, 1:2], by = "variable")
col_occ$label <- as_factor(str_replace(col_occ$label, ".*!!(.*)", "\\1"))

# Graph occupation data to see differences in zip codes

## Order the x-axis of the graph based on occupation. 
### We group essential occupations together and look
### at their proportion of the zip code and 
### sort in ascending order.
col_occ_ess <- col_occ %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(str_detect(label,
                    "(Natural.*)|(Production.*)")) %>% 
  mutate(prop_ess = sum(prop)) %>% 
  select(GEOID, prop_ess) %>% 
  distinct(GEOID, prop_ess)

lvls_col_ess <- col_occ_ess %>% 
  arrange(prop_ess) %>% 
  pull(GEOID)

ggplot(col_occ) +
  geom_col(aes(factor(GEOID, levels = lvls_col_ess), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## ANOVA TEST
col_occ_ess <- left_join(col_occ_ess, col_covid, by = c("GEOID" = "ZIP"))

ess_lm <- lm(average ~ prop_ess, data = col_occ_ess)
summary(ess_lm)
anova(ess_lm)
#No evidence is deteched from the anova test of significance. P-value is 0.3099

#################################
# Public Transportation
#################################

col_trans <- get_acs(geography = "zcta",
                       table = "B08301") %>% 
  filter(GEOID %in% zip_code_col$zip,
         variable %in% c("B08301_002", str_c("B08301_0", c(10, 16:21))))

col_trans <- left_join(col_trans, variables_2018[, 1:2], by = "variable")
col_trans$label <- as_factor(str_replace(col_trans$label, ".*!!(.*)", "\\1"))

col_trans <- col_trans %>% 
  mutate(use_public = if_else(str_detect(label, "^Pub.*")|str_detect(label, "Taxicab"), 
                              TRUE, FALSE))

## Graph it in a geom_col plot

col_trans_public <- col_trans %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(use_public == TRUE) %>% 
  mutate(prop_public = sum(prop)) 

lvls_trans <- col_trans_public %>% 
  arrange(prop_public) %>% 
  distinct(GEOID) %>% 
  pull(GEOID)

ggplot(col_trans) +
  geom_col(aes(factor(GEOID, levels = lvls_trans), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## ANOVA TEST
col_trans_public <- left_join(col_trans_public, col_covid, by = c("GEOID" = "ZIP"))

trans_lm <- lm(average ~ prop_public, data = col_trans_public)
summary(trans_lm)
anova(trans_lm)
#Our p-value of 0.00303 suggests public transportation and COVID-19 cases are significant to either other.

###################################
# Citizenship
###################################

col_citizen_clean <- get_acs(geography = "zcta",
                               table = "B05001") %>% 
  filter(GEOID %in% zip_code_col$zip,
         variable %in% str_c("B05001_00", 2:6))

col_citizen <- left_join(col_citizen_clean, variables_2018[, 1:2], by = "variable")
col_citizen$label <- as_factor(str_replace(col_citizen$label, ".*!!(.*)", "\\1"))

col_citizen <- col_citizen %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n)

## Create bar graph with breakdown of citizenship for each zip

col_citizen_prop_us <- col_citizen %>% 
  filter(str_detect(label,
                    "^U.S.")) %>% 
  mutate(prop_us_citizen = sum(prop)) %>% 
  select(GEOID, prop_us_citizen) %>% 
  distinct(GEOID, prop_us_citizen)

lvls_col_citizen <- col_citizen_prop_us %>% 
  arrange(prop_us_citizen) %>% 
  pull(GEOID)

ggplot(col_citizen) +
  geom_col(aes(factor(GEOID, levels = lvls_col_citizen), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## ANOVA TEST
col_citizen_prop_us <- left_join(col_citizen_prop_us, col_covid, by = c("GEOID" = "ZIP"))

citizen_lm <- lm(average ~ prop_us_citizen, data = col_citizen_prop_us)
summary(citizen_lm)
anova(citizen_lm)
#With a p-value of 0.3049, we do not see a significance of citizenship and COVID-19 cases.

##################################
# Race
##################################

col_race_clean <- get_acs(geography = "zcta",
                            table = "B02001") %>% 
  filter(GEOID %in% zip_code_col$zip,
         variable %in% str_c("B02001_00", 2:8))

col_race <- left_join(col_race_clean, variables_2018[, 1:2], by = "variable")
col_race$label <- as_factor(str_replace(col_race$label, ".*!!(.*)", "\\1"))

col_race <- col_race %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n)

## Graph race proportions by zip code in bar graph

lvls_col_race <- col_race %>% 
  filter(str_detect(label, "White alone")) %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(col_race) +
  geom_col(aes(factor(GEOID, levels = lvls_col_race), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## ANOVA TEST 
col_race <- left_join(col_race, col_covid, by = c("GEOID" = "ZIP"))

race_lm <- lm(average ~ prop, data = col_race)
summary(race_lm)
anova(race_lm)
#We have a p-value of 1

