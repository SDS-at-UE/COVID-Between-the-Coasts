# Indianapolis EDA
# Exploring demographic data for Indianapolis
# Income, private vs public HI, Gini index (chicago), public transportation (chicago),
#   occupation (essential vs non-essential), citizenship, race


library(tidycensus)
library(tidyverse)
library(readxl)
library(sf)

source("Scripts/functions.R")

zip_code <- read_csv("Data/zip_county.csv")
zip_code_indy <- zip_code %>% filter(primary_city == "Indianapolis",
                                     state == "IN")
# Indy COVID data
indiana_cases_by_zip <- read_excel("Data/indiana_cases_by_zip.xlsx")
indiana_cases_by_zip <- indiana_cases_by_zip %>% filter(ZIP_CD %in% zip_code_indy$zip)
indiana_cases_by_zip$PATIENT_COUNT <- as.numeric(indiana_cases_by_zip$PATIENT_COUNT)
indiana_cases_by_zip$POPULATION <- as.numeric(indiana_cases_by_zip$POPULATION)
indiana_cases_by_zip$ZIP_CD <- as.numeric(indiana_cases_by_zip$ZIP_CD)
indiana_cases_by_zip <- indiana_cases_by_zip %>% 
  mutate(case_rate = PATIENT_COUNT/POPULATION) 

# Variables from the ACS survey
variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>%
  rename(variable = name)

# Income data
indy_income <- get_acs(geography = "zcta",
                       year = 2018,
                       table = "B19101") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B19101_001"))
indy_income <- left_join(indy_income, variables_2018[, 1:2], by = "variable")
indy_income$label <- as_factor(str_replace(indy_income$label, ".*!!(.*)", "\\1"))

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

indy_income$GEOID <- as.numeric(indy_income$GEOID)
indy_income <- left_join(indy_income, indiana_cases_by_zip, by = c("GEOID" = "ZIP_CD"))

ggplot(indy_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Income Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Income Level")

## Test Correlation between percentage of population making 6-figure incomes to the 
## case rate of COVID
indy_income_six_figure$GEOID <- as.numeric(indy_income_six_figure$GEOID)
indy_income_six_figure_corr <- left_join(indy_income_six_figure, 
                                         indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_income_six_figure$prop_100K, 
         indy_income_six_figure_corr$case_rate, use = "complete.obs")

# Scatterplot - one outlier (46204)
ggplot(indy_income_six_figure_corr) +
  geom_point(aes(prop_100K, case_rate))

filter(indy_income_six_figure_corr, case_rate > .03)

indy_income_cor_46204 <- indy_income_six_figure_corr %>% 
  filter(case_rate < .03)

cor.test(indy_income_cor_46204$prop_100K, 
         indy_income_cor_46204$case_rate, use = "complete.obs")
# After you remove the outlier you get a strong negative correlation. 


# Occupation
indy_occ <- get_acs(geography = "zcta",
                    year = 2018,
                    table = "C24060") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("C24060_001"))
indy_occ <- left_join(indy_occ, variables_2018[, 1:2], by = "variable")
indy_occ$label <- as_factor(str_replace(indy_occ$label, ".*!!(.*)", "\\1"))

## Essential
indy_occ_essential <- indy_occ %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Natural resources, construction, and maintenance occupations",
                      "Production, transportation, and material moving occupations")) %>% 
  mutate(prop_essential = sum(prop)) %>% 
  select(GEOID, prop_essential) %>% 
  distinct(GEOID, prop_essential)

lvls3 <- indy_occ_essential %>% 
  arrange(prop_essential) %>% 
  pull(GEOID)

ggplot(indy_occ) +
  geom_col(aes(factor(GEOID, levels = lvls3), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Occupation Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Occupation Category")


## Test Correlation between percentage of population that have essential occupations to 
## the case rate of COVID
indy_occ_essential$GEOID <- as.numeric(indy_occ_essential$GEOID)
indy_occ_cor <- left_join(indy_occ_essential, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_occ_cor$prop_essential, indy_occ_cor$case_rate, use = "complete.obs")

## Non-essential 
indy_occ_nonessential <- indy_occ %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Management, business, science, and arts occupations",
                      "Service occupations",
                      "Sales and office occupations",
                      "Employee of private company workers")) %>% 
  mutate(prop_nonessential = sum(prop)) %>% 
  select(GEOID, prop_nonessential) %>% 
  distinct(GEOID, prop_nonessential)

lvls4 <- indy_occ_nonessential %>% 
  arrange(prop_nonessential) %>% 
  pull(GEOID)

ggplot(indy_occ) +
  geom_col(aes(factor(GEOID, levels = lvls4), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Occupation Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Occupation Category")

## testing  correlation
indy_occ_nonessential$GEOID <- as.numeric(indy_occ_nonessential$GEOID)
indy_occ_nonessential_cor <- left_join(indy_occ_nonessential, indiana_cases_by_zip,
                                       by = c("GEOID"="ZIP_CD"))
cor.test(indy_occ_nonessential_cor$prop_nonessential, 
         indy_occ_nonessential_cor$case_rate, use = "complete.obs")

ggplot(indy_occ_nonessential_cor, aes(prop_nonessential, case_rate)) +
  geom_point() +
  labs(title = "Proportion of ZIP Code Deemed Non-Essential vs. Case Rate")

indy_occ_nonessential_cor_46204 <- indy_occ_nonessential_cor %>% 
  filter(case_rate < .03)
cor.test(indy_occ_nonessential_cor_46204$prop_nonessential, indy_occ_nonessential_cor_46204$case_rate, 
         use = "complete.obs")

indy_occ_essential$GEOID <- as.numeric(indy_occ_essential$GEOID)
indy_occ_essential_cor <- left_join(indy_occ_essential, indiana_cases_by_zip,
                                    by = c("GEOID"="ZIP_CD"))
cor.test(indy_occ_essential_cor$prop_essential, 
         indy_occ_essential_cor$case_rate, use = "complete.obs")

ggplot(indy_occ_cor, aes(prop_essential, case_rate)) +
  geom_point() +
  labs(title = "Proportion of ZIP Code Deemed Essential vs. Case Rate")

indy_occ_cor_46204 <- indy_occ_cor %>% 
  filter(case_rate < .03)
cor.test(indy_occ_cor_46204$prop_essential, indy_occ_cor_46204$case_rate, 
         use = "complete.obs")

# Age
## Looked at people over the age of 65
indy_sex <- get_acs(geography = "zcta",
                    year = 2018,
                    table = "B01001") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B01001_001"))
indy_sex <- left_join(indy_sex, variables_2018[, 1:2], by = "variable")
indy_sex$label <- as_factor(str_replace(indy_sex$label, ".*!!(.*)", "\\1"))

indy_over_sixty5 <- indy_sex %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("65 and 66 years",
                      "67 to 69 years",
                      "70 to 74 years",
                      "75 to 79 years",
                      "80 to 84 years",
                      "85 years and over")) %>% 
  mutate(prop_65 = sum(prop)) %>% 
  select(GEOID, prop_65) %>% 
  distinct(GEOID, prop_65)

lvls5 <- indy_over_sixty5 %>% 
  arrange(prop_65) %>% 
  pull(GEOID)

ggplot(indy_sex) +
  geom_col(aes(factor(GEOID, levels = lvls5), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Age Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Age")

## Testing correlation
indy_over_sixty5$GEOID <- as.numeric(indy_over_sixty5$GEOID)
indy_sex_cor <- left_join(indy_over_sixty5, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_sex_cor$prop_65, indy_sex_cor$case_rate, use = "complete.obs")

indy_sex_cor_46204 <- indy_sex_cor %>% 
  filter(case_rate < .03)
cor.test(indy_sex_cor_46204$prop_65, indy_sex_cor_46204$case_rate, 
         use = "complete.obs")

# Race
indy_race <- get_acs(geography = "zcta",
                     year = 2018,
                     table = "B02001") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B02001_001"))
indy_race <- left_join(indy_race, variables_2018[, 1:2], by = "variable")
indy_race$label <- as_factor(str_replace(indy_race$label, ".*!!(.*)", "\\1"))

## african american analysis
indy_black <- indy_race %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Black or African American alone")) %>% 
  mutate(prop_black = sum(prop)) %>% 
  select(GEOID, prop_black) %>% 
  distinct(GEOID, prop_black)

lvls6 <- indy_black %>% 
  arrange(prop_black) %>% 
  pull(GEOID)

ggplot(indy_race) +
  geom_col(aes(factor(GEOID, levels = lvls6), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Race Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Race")

## correlation test
indy_black$GEOID <- as.numeric(indy_black$GEOID)
indy_race_cor <- left_join(indy_black, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_race_cor$prop_black, indy_race_cor$case_rate, use = "complete.obs")
# Scatterplot
ggplot(indy_race_cor) +
  geom_point(aes(prop_black, case_rate))

filter(indy_race_cor, case_rate > .03)
indy_race_cor_46204 <- filter(indy_race_cor, case_rate < .03)
cor.test(indy_race_cor_46204$prop_black, indy_race_cor_46204$case_rate, use = "complete.obs")

# Citizenship 
indy_citizen <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B05001") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B05001_001"))
indy_citizen <- left_join(indy_citizen, variables_2018[, 1:2], by = "variable")
indy_citizen$label <- as_factor(str_replace(indy_citizen$label, ".*!!(.*)", "\\1"))

indy_not_citizen <- indy_citizen %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Not a U.S. citizen")) %>% 
  mutate(prop_not_citizen = sum(prop)) %>% 
  select(GEOID, prop_not_citizen) %>% 
  distinct(GEOID, prop_not_citizen)

lvls7 <- indy_not_citizen %>% 
  arrange(prop_not_citizen) %>% 
  pull(GEOID)

ggplot(indy_citizen) +
  geom_col(aes(factor(GEOID, levels = lvls7), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Citizen Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Citizenship Status")

## correlation test
indy_not_citizen$GEOID <- as.numeric(indy_not_citizen$GEOID)
indy_citizen_cor <- left_join(indy_not_citizen, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_citizen_cor$prop_not_citizen, indy_citizen_cor$case_rate, use = "complete.obs")

# Scatterplot
ggplot(indy_citizen_cor) + 
  geom_point(aes(prop_not_citizen,case_rate))

filter(indy_citizen_cor, case_rate > .03)
indy_citizen_cor_46204 <- filter(indy_citizen_cor, case_rate < .03)
cor.test(indy_citizen_cor_46204$prop_not_citizen, indy_citizen_cor_46204$case_rate, 
         use = "complete.obs")

# Public transportation
indy_trans <- get_acs(geography = "zcta",
                         year = 2018,
                         table = "B08301") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         !variable %in% c("B08301_001"))
indy_trans <- left_join(indy_trans, variables_2018[, 1:2], by = "variable")
indy_trans$label <- as_factor(str_replace(indy_trans$label, ".*!!(.*)", "\\1"))

indy_public_trans <- indy_trans %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Public transportation (excluding taxicab)",
                      "Bus or trolley bus",
                      "Streetcar or trolley car (carro publico in Puerto Rico)",
                      "Subway or elevated",
                      "Railroad")) %>% 
  mutate(prop_public = sum(prop)) %>% 
  select(GEOID, prop_public) %>% 
  distinct(GEOID, prop_public)

lvls8 <- indy_public_trans %>% 
  arrange(prop_public) %>% 
  pull(GEOID)

ggplot(indy_trans) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Transportation Distribution for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Means of Transportation")

## correlation test
indy_public_trans$GEOID <- as.numeric(indy_public_trans$GEOID)
indy_trans_cor <- left_join(indy_public_trans, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_trans_cor$prop_public, indy_trans_cor$case_rate, use = "complete.obs")

## Scatterplot 
ggplot(indy_trans_cor) +
  geom_point(aes(prop_public, case_rate))

filter(indy_trans_cor, case_rate > .03)
indy_trans_cor_46204 <- filter(indy_trans_cor, case_rate < .03)
cor.test(indy_trans_cor_46204$prop_public, indy_gini_cor_46204$case_rate, use = "complete.obs")
# Correlation goes way down 

# Gini Index 
indy_gini <- get_acs(geography = "zcta",
                      year = 2018,
                      table = "B19083") %>% 
  filter(GEOID %in% zip_code_indy$zip,
         variable %in% c("B19083_001"))
indy_gini <- left_join(indy_gini, variables_2018[, 1:2], by = "variable")
indy_gini$label <- as_factor(str_replace(indy_gini$label, ".*!!(.*)", "\\1"))

ggplot(indy_gini) +
  geom_col(mapping = aes(x = factor(GEOID), y = estimate)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Gini Index Value for Each Indianapolis Zip Code",
       x = "Zip Code",
       y = "Gini Index Value")

## correlation test
indy_gini$GEOID <- as.numeric(indy_gini$GEOID)
indy_gini_cor <- left_join(indy_gini, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD"))
cor.test(indy_gini$estimate, indy_gini_cor$case_rate, use = "complete.obs")

## Scatterplot 
ggplot(indy_gini_cor) +
  geom_point(aes(estimate, case_rate))

filter(indy_gini_cor, case_rate > .03)
indy_gini_cor_46204 <- filter(indy_gini_cor, case_rate < .03)
cor.test(indy_gini_cor_46204$estimate, indy_gini_cor_46204$case_rate, use = "complete.obs")
# Correlation goes way down

# Private vs. Public HI

indy_hi_private <- get_acs(geography = "zcta",
                            table = "B27002") %>% 
  filter(GEOID %in% zip_code_indy$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable") %>% 
  filter(str_count(label, "!!") >= 4)

indy_hi_public <- get_acs(geography = "zcta",
                           table = "B27003") %>% 
  filter(GEOID %in% zip_code_indy$zip) %>% 
  left_join(variables_2018[, 1:2], by = "variable") %>% 
  filter(str_count(label, "!!") >= 4)

indy_hi_private$label <- str_remove(indy_hi_private$label, "Estimate!!Total!!")
indy_hi_public$label <- str_remove(indy_hi_public$label, "Estimate!!Total!!")

indy_hi_private <- separate(indy_hi_private,
                             label,
                             sep = "!!",
                             into = c("Sex", "Age", "Private_HI"))
indy_hi_public <- separate(indy_hi_public,
                            label,
                            sep = "!!",
                            into = c("Sex", "Age", "Public_HI"))

indy_hi_private$Private_HI <- if_else(indy_hi_private$Private_HI == "No private health insurance", 
                                      "No", "Yes")
indy_hi_public$Public_HI <- if_else(indy_hi_public$Public_HI == "No public coverage", "No", "Yes")

indy_hi_private$Sex <- as_factor(indy_hi_private$Sex)
indy_hi_public$Sex <- as_factor(indy_hi_public$Sex)
indy_hi_private$Age <- as_factor(indy_hi_private$Age)
indy_hi_public$Age <- as_factor(indy_hi_public$Age)
indy_hi_private$Private_HI <- as_factor(indy_hi_private$Private_HI)
indy_hi_public$Public_HI <- as_factor(indy_hi_public$Public_HI) 

## Look at Private HI Coverage

indy_hi_private_2 <- indy_hi_private %>% group_by(GEOID, Private_HI) %>% 
  summarize(count = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  mutate(pop = sum(count),
         prop = count/pop)

### Graph Private HI Coverage by proportion

lvls_indy_hi_private <- indy_hi_private_2 %>% 
  filter(Private_HI == "Yes") %>% 
  arrange(prop) %>% 
  pull(GEOID)

ggplot(indy_hi_private_2) +
  geom_col(aes(factor(GEOID, levels = lvls_indy_hi_private), count, fill = Private_HI),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Scatterplot Private HI vs case rate
indy_hi_private_2$GEOID <- as.numeric(indy_hi_private_2$GEOID)
indy_hi_private_cor <- left_join(indy_hi_private_2, indiana_cases_by_zip, by = c("GEOID"="ZIP_CD")) %>% 
  filter(Private_HI == "Yes")

indy_hi_private_cor %>%
  ggplot(aes(prop, case_rate)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = GEOID)) +
  labs(title = "Private HI vs Case Rate")
## There is no groupings but there is the outlier

indy_hi_private_cor_46204 <- indy_hi_private_cor %>% 
  filter(case_rate < .03)
cor.test(indy_hi_private_cor_46204$prop, indy_hi_private_cor_46204$case_rate, 
         use = "complete.obs")
# Pretty strong negative correlation





























