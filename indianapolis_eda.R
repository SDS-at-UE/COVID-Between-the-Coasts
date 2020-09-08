# Indianapolis EDA
## Exploring demographic data for Indianapolis

library(tidycensus)
library(tidyverse)

source("Scripts/functions.R")

######################################
# Retrieve zip code to county dataset
######################################

zip_code <- read_csv("Data/zip_county.csv")
zip_code_indy <- zip_code %>% filter(primary_city == "Indianapolis",
                                      state == "IN")

### Looking at total population in indy ###
indy_data_pop <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B00001",
                        geometry = TRUE) %>%   
                        filter(GEOID %in% zip_code_indy$zip)

# Filtering to indianapolis, only has 37 of the zipcodes instead of 65

indy_data_pop <- left_join(indy_data_pop, variables_2018[, 1:2], by = "variable")

# Was able to download Indiana cases by zip. However some of the data was suppressed. 
# POPULATION = population count as of 2010
# PERCENTAGE = percentage of COVID cases to population in a given zip code 

indiana_cases_by_zip <- read_excel("Data/indiana_cases_by_zip.xlsx")
# Filtering the cases by zip to the zip codes just for Indianapolis. 
indiana_cases_by_zip <- indiana_cases_by_zip %>% filter(ZIP_CD %in% zip_code_indy$zip)

# Also had 37 zip codes so that matches up nicely with the indy_data dataset. Additionally, only one
# zip code was suppressed that were looking at so that is good. The indiana_cases_by_zip already has
# the population in it so I did not need to do what I did above to the indy_data data_set. 

### INCOME DATA EXPLORATION ###
# Looking at the median income in the past 12 months

indy_data_income <- get_acs(geography = "zcta",
                         year = 2018,
                         table = "B19101",
                         geometry = TRUE) %>% 
                         filter(GEOID %in% zip_code_indy$zip, !variable %in% c("B19101_001"))

indy_data_income <- left_join(indy_data_income, variables_2018[, 1:2], by = "variable")
indy_data_income$label <- as_factor(str_replace(indy_data_income$label, ".*!!(.*)", "\\1"))

indy_data_income <- indy_data_income[,-c(2,5)] # got rid of the extra zipcode and moe columns

# Making each income level it's own row then combining all the datasets
less_than_10 <- indy_data_income %>% filter(label == "Less than $10,000") 
colnames(less_than_10)[3] <- ("< $10,000")

ten_to_14 <- indy_data_income %>% filter(label == "$10,000 to $14,999") 
colnames(ten_to_14)[3] <- ("$10,000 to $14,999")

fifteen_to_20 <- indy_data_income %>% filter(label == "$15,000 to $19,999") 
colnames(fifteen_to_20)[3] <- ("$15,000 to $19,999")

twenty_to_25 <- indy_data_income %>% filter(label == "$20,000 to $24,999") 
colnames(twenty_to_25)[3] <- ("$20,000 to $24,999")

twentyfive_to_30 <- indy_data_income %>% filter(label == "$25,000 to $29,999") 
colnames(twentyfive_to_30)[3] <- ("$25,000 to $29,999")

thirty_to_35 <- indy_data_income %>% filter(label == "$30,000 to $34,999") 
colnames(thirty_to_35)[3] <- ("$30,000 to $34,999")

thirtyfive_to_40 <- indy_data_income %>% filter(label == "$35,000 to $39,999") 
colnames(thirtyfive_to_40)[3] <- ("$35,000 to $39,999")

forty_to_45 <- indy_data_income %>% filter(label == "$40,000 to $44,999") 
colnames(forty_to_45)[3] <- ("$40,000 to $44,999")

fortyfive_to_50 <- indy_data_income %>% filter(label == "$45,000 to $49,999") 
colnames(fortyfive_to_50)[3] <- ("$45,000 to $49,999")

fifty_to_60 <- indy_data_income %>% filter(label == "$50,000 to $59,999") 
colnames(fifty_to_60)[3] <- ("$50,000 to $59,999")

sixty_to_75 <- indy_data_income %>% filter(label == "$60,000 to $74,999") 
colnames(sixty_to_75)[3] <- ("$60,000 to $74,999")

seventyfive_to_100 <- indy_data_income %>% filter(label == "$75,000 to $99,999") 
colnames(seventyfive_to_100)[3] <- ("$75,000 to $99,999")

onehundred_to_125 <- indy_data_income %>% filter(label == "$100,000 to $124,999") 
colnames(onehundred_to_125)[3] <- ("$100,000 to $124,999")

onehundred25_to_150 <- indy_data_income %>% filter(label == "$125,000 to $149,999") 
colnames(onehundred25_to_150)[3] <- ("$125,000 to $149,999")

onehundred50_to_200 <- indy_data_income %>% filter(label == "$150,000 to $199,999") 
colnames(onehundred50_to_200)[3] <- ("$150,000 to $199,999")

greater_than_200 <- indy_data_income %>% filter(label == "$200,000 or more") 
colnames(greater_than_200)[3] <- ("> $200,000")

# Joining all the data sets into one by zip code
merge1 <- inner_join(less_than_10 %>% as.data.frame(), ten_to_14 %>% as.data.frame(), by = "GEOID")
merge2 <- inner_join(merge1 %>% as.data.frame(), fifteen_to_20 %>% as.data.frame(), by = "GEOID")
merge3 <- inner_join(merge2 %>% as.data.frame(), twenty_to_25 %>% as.data.frame(), by = "GEOID")
merge4 <- inner_join(merge3 %>% as.data.frame(), twentyfive_to_30 %>% as.data.frame(), by = "GEOID")
merge5 <- inner_join(merge4 %>% as.data.frame(), thirty_to_35 %>% as.data.frame(), by = "GEOID")
merge6 <- inner_join(merge5 %>% as.data.frame(), thirtyfive_to_40 %>% as.data.frame(), by = "GEOID")
merge7 <- inner_join(merge6 %>% as.data.frame(), forty_to_45 %>% as.data.frame(), by = "GEOID")
merge8 <- inner_join(merge7 %>% as.data.frame(), fortyfive_to_50 %>% as.data.frame(), by = "GEOID")
merge9 <- inner_join(merge8 %>% as.data.frame(), fifty_to_60 %>% as.data.frame(), by = "GEOID")
merge10 <- inner_join(merge9 %>% as.data.frame(), sixty_to_75 %>% as.data.frame(), by = "GEOID")
merge11 <- inner_join(merge10 %>% as.data.frame(), seventyfive_to_100 %>% as.data.frame(), by = "GEOID")
merge12 <- inner_join(merge11 %>% as.data.frame(), onehundred_to_125 %>% as.data.frame(), by = "GEOID")
merge13 <- inner_join(merge12 %>% as.data.frame(), onehundred25_to_150 %>% as.data.frame(), by = "GEOID")
merge14 <- inner_join(merge13 %>% as.data.frame(), onehundred50_to_200 %>% as.data.frame(), by = "GEOID")
## Final dataset 
indy_income_final <- inner_join(merge14 %>% as.data.frame(), greater_than_200 %>% as.data.frame(), 
                                by = "GEOID") 
# Cleaning it up 
indy_income_final <- indy_income_final[,-c(2,4,5,6,8,9,10,12,13,14,16,17,18,20,21,22,24,25,26,28,29,30,32,
                                           33,34,36,37,38,40,41,42,44,45,46,48,49,50)]
indy_income_final <- indy_income_final[,-c(15,16,17,19,20,21,23,24,25,27,28)]

## putting the income level into a different data set that has 3 brackets (low,med,high)
## determined by indystar article

indy_income_brackets <- indy_income_final %>% mutate(low = `< $10,000` + `$10,000 to $14,999` +
                                                     `$15,000 to $19,999` + `$20,000 to $24,999` +
                                                     `$25,000 to $29,999` + `$30,000 to $34,999` +
                                                     `$35,000 to $39,999`,
                                                     medium = `$40,000 to $44,999` + `$45,000 to $49,999` +
                                                     `$50,000 to $59,999` + `$60,000 to $74,999` +
                                                       `$75,000 to $99,999`,
                                                     high = `$100,000 to $124,999` + `$125,000 to $149,999`
                                                     + `$150,000 to $199,999` + `> $200,000`)
indy_income_brackets <- indy_income_brackets[,-c(2:17)]
# Combining the income brackets with the coronovirus data
colnames(indiana_cases_by_zip)[1] <- ("GEOID")
income_and_covid <- merge(indy_income_brackets, indiana_cases_by_zip, by = "GEOID")

## Want to maybe look at populate > 65% and some race demographic %'s and add those columns to the 
#  income and covid data set. 
# since they match up to be able to view everything at once. 

## race
indy_race <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B02001") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B02001_001"))
indy_race <- left_join(indy_race, variables_2018[, 1:2], by = "variable")
indy_race$label <- as_factor(str_replace(indy_race$label, ".*!!(.*)", "\\1"))

ggplot(indy_race) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

indy_white_race <- get_acs(geography = "zcta",
                         year = 2018,
                         table = "B02001",
                         geometry = TRUE) %>%   
                         filter(GEOID %in% zip_code_indy$zip, variable %in% c("B02001_002"))
colnames(indy_white_race)[4] <- ("white estimate")

indy_black_race <- get_acs(geography = "zcta",
                           year = 2018,
                           table = "B02001",
                           geometry = TRUE) %>%   
                           filter(GEOID %in% zip_code_indy$zip, variable %in% c("B02001_003"))
colnames(indy_black_race)[4] <- ("black estimate")

indy_asian_race <- get_acs(geography = "zcta",
                           year = 2018,
                           table = "B02001",
                           geometry = TRUE) %>%   
                           filter(GEOID %in% zip_code_indy$zip, variable %in% c("B02001_004"))
colnames(indy_asian_race)[4] <- ("asian estimate")

## merging the 3 race (black, white, asian) estimates into one data set 
merge20 <- inner_join(indy_white_race %>% as.data.frame(), 
                      indy_black_race %>% as.data.frame(), by = "GEOID")

indy_race_dem <- inner_join(merge20 %>% as.data.frame(), 
                           indy_asian_race %>% as.data.frame(), by = "GEOID")

indy_race_dem <- indy_race_dem[,-c(2,3,5,6,7,8,10,11,12,13,15,16)] # got rid of unnecesary columns

# combining the race to the indy income/covid data set 
race_income_covid <- merge(indy_race_dem, income_and_covid, by = "GEOID")

### Age demographics (Looked at 75 and up)
seventy5_to_79 <- get_acs(geography = "zcta",
                         year = 2018,
                         table = "B01001",
                         geometry = TRUE) %>%   
                         filter(GEOID %in% zip_code_indy$zip, variable %in% c("B01001_023"))
colnames(seventy5_to_79)[4] <- ("75_to_79")

eighty_to_84 <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B01001",
                          geometry = TRUE) %>%   
                          filter(GEOID %in% zip_code_indy$zip, variable %in% c("B01001_024"))
colnames(eighty_to_84)[4] <- ("80_to_84")

eighty5_and_up <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B01001",
                        geometry = TRUE) %>%   
                        filter(GEOID %in% zip_code_indy$zip, variable %in% c("B01001_025"))
colnames(eighty5_and_up)[4] <- ("85_and_up")

# merging the above 3 datasets (they're male numbers only) and cleaning it up 
merge30 <- inner_join(seventy5_to_79 %>% as.data.frame(), eighty_to_84 %>% as.data.frame(), by = "GEOID")
male_age <- inner_join(merge30 %>% as.data.frame(), eighty5_and_up %>% as.data.frame(), by = "GEOID")

male_age <- male_age[,-c(2,3,5,6,7,8,10,11,12,13,15,16)]
male_age <- male_age %>% mutate(total_75_and_up = `75_to_79` + `80_to_84` + `85_and_up`)
male_age <- male_age[,-c(2:4)]

# Now doing female 
seventy5_to_79F <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B01001",
                          geometry = TRUE) %>%   
                          filter(GEOID %in% zip_code_indy$zip, variable %in% c("B01001_047"))
colnames(seventy5_to_79F)[4] <- ("75_to_79F")

eighty_to_84F <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B01001",
                        geometry = TRUE) %>%   
                        filter(GEOID %in% zip_code_indy$zip, variable %in% c("B01001_048"))
colnames(eighty_to_84F)[4] <- ("80_to_84F")

eighty5_and_upF <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B01001",
                          geometry = TRUE) %>%   
                          filter(GEOID %in% zip_code_indy$zip, variable %in% c("B01001_049"))
colnames(eighty5_and_upF)[4] <- ("85_and_upF")

# merging the above 3 datasets (they're male numbers only) and cleaning it up 
merge40 <- inner_join(seventy5_to_79F %>% as.data.frame(), eighty_to_84F %>% as.data.frame(), by = "GEOID")
female_age <- inner_join(merge40 %>% as.data.frame(), eighty5_and_upF %>% as.data.frame(), by = "GEOID")

female_age <- female_age[,-c(2,3,5,6,7,8,10,11,12,13,15,16)]
female_age <- female_age %>% mutate(total_75_and_upF = `75_to_79F` + `80_to_84F` + `85_and_upF`)
female_age <- female_age[,-c(2:4)]

# Combining male and female age (75 and up)
indy_age <- merge(male_age,female_age, by = "GEOID")
indy_age <- indy_age %>% mutate(aged_75_and_up = total_75_and_up + total_75_and_upF)
indy_age <- indy_age[,-c(2,3)]

# Combining with the race, income and covid data
age_race_income_covid <- merge(indy_age, race_income_covid, by = "GEOID")

## Converting the demographics to be proportions 
age_race_income_covid$POPULATION <- as.numeric(age_race_income_covid$POPULATION)
age_race_income_covid <- age_race_income_covid %>% mutate(`white estimate` = `white estimate`/POPULATION,
                                                          `black estimate` = `black estimate`/POPULATION,
                                                          `asian estimate` = `asian estimate`/POPULATION,
                                                          aged_75_and_up = aged_75_and_up/POPULATION,
                                                          high = high/POPULATION,
                                                          medium = medium/POPULATION,
                                                          low = low/POPULATION)


## Statistics and graphs
age_race_income_covid$PERCENTAGE <- as.numeric(age_race_income_covid$PERCENTAGE)
age_race_income_covid <- age_race_income_covid[-37,]  ## Got rid of the suppressed zip code 
mean(age_race_income_covid$PERCENTAGE) ## 1.85% = average % of the population in that zip code that has covid

white <- age_race_income_covid %>% filter(`white estimate` > .50) 
mean(white$PERCENTAGE) ## case rate for zip codes that are > 50% white

black <- age_race_income_covid %>% filter(`black estimate` > .50) 
mean(black$PERCENTAGE) ## case rate for zip codes that are > 50% black

ggplot(data = age_race_income_covid) + 
  stat_summary(
    mapping = aes(x = GEOID, y = PATIENT_COUNT),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# Corelation test age above 75 and percentage: NOT SIGNIFICANT
cor.test(age_race_income_covid$aged_75_and_up, age_race_income_covid$PERCENTAGE, use = "complete.obs")

# Corelation test of black proportion and percentage: SIGNIFICANT 
cor.test(age_race_income_covid$`black estimate`, age_race_income_covid$PERCENTAGE, use = "complete.obs")

# Corelation test of whites proportion and percentage: NOT SIGNIFICANT 
cor.test(age_race_income_covid$`white estimate`, age_race_income_covid$PERCENTAGE, use = "complete.obs")

# Corelation test of asian proportion and percentage: NOT SIGNIFICANT 
cor.test(age_race_income_covid$`asian estimate`, age_race_income_covid$PERCENTAGE, use = "complete.obs")

# Corelation test of high income proportion and percentage: NOT SIGNIFICANT 
cor.test(age_race_income_covid$high, age_race_income_covid$PERCENTAGE, use = "complete.obs")

# Corelation test of low income proportion and percentage: NOT SIGNIFICANT 
cor.test(age_race_income_covid$low, age_race_income_covid$PERCENTAGE, use = "complete.obs")

# Corelation test of medium income proportion and percentage: SIGNIFICANT 
cor.test(age_race_income_covid$medium, age_race_income_covid$PERCENTAGE, use = "complete.obs")

######################################
# Retrieve zip code to county dataset
######################################

zip_code <- read_csv("Data/zip_county.csv")
zip_code_indy <- zip_code %>% filter(primary_city == "Indianapolis",
                                     state == "IN")

######################################
# Retrieve COVID case data for Indianapolis
# collected from 
#####################################

indy_covid <- read_xlsx("Data/indiana_cases_by_zip.xlsx")
indy_covid <- indy_covid %>% rename(zip = ZIP_CD,
                                    population = POPULATION,
                                    cases = PATIENT_COUNT,
                                    percentage = PERCENTAGE)

indy_covid$zip <- as.character(indy_covid$zip)
indy_covid$cases <- as.numeric(indy_covid$cases)
indy_covid$population <- as.numeric(indy_covid$population)
indy_covid <- indy_covid %>% mutate(case_rate = cases/population)
######################################
# Retrieve Income data
######################################

indy_income <- get_acs(geography = "zcta",
                       table = "B19101",
                       geometry = TRUE) %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B19101_001"))

indy_income <- left_join(indy_income, variables_2018[, 1:2], by = "variable")

indy_income$label <- as_factor(str_replace(indy_income$label, ".*!!(.*)", "\\1"))

# Graph income data to see differences in zip codes

## Order the x-axis of the graph based on income. 
### We group 6-figure incomes together and look
### at their proportion of the zip code and 
### sort in ascending order.
indy_income_six_figure <- indy_income %>% 
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

lvls <- indy_income_six_figure %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

ggplot(indy_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
### Test Correlation between percentage of population making 6-figure
### incomes to the case rate of COVID
#### RESULT = Not significant 
indy_income_cor <- left_join(indy_income_six_figure, indy_covid, by = c("GEOID"="zip"))
cor.test(indy_income_cor$prop_100K, indy_income_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code

indy_income <- left_join(indy_income, indy_covid, by = c("GEOID" = "zip"))


pal <- colorNumeric(palette = "viridis", domain = indy_income$case_rate)

indy_income %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(popup = str_c("<strong>", indy_income$GEOID,
                            "</strong><br /> Case Rate ", indy_income$case_rate),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(case_rate)) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~ case_rate,
            title = "Case Rate (per 100000)",
            opacity = 1)


