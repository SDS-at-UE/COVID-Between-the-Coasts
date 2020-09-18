# Chicago EDA
## Exploring demographic data for Chicago

library(tidycensus)
library(tidyverse)
library(sf)


#Read in zip code data
zip_data <- read_csv("zip_data.csv", 
                     col_types = cols(zip = col_character()))

zip_data <- zip_data %>% select(zip, type, primary_city, acceptable_cities, state, county, latitude, longitude)
write_csv(zip_data, "zip_county.csv")

#Read in chicago covid data
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

#------------------------------------------------------------------------------------------
### Family Income in the Past 12 Months
chicago_income <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B19101") %>% 
                  filter(GEOID %in% zip_code_chicago$zip,
                         !variable %in% c("B19101_001"))
chicago_income <- left_join(chicago_income, variables_2018[, 1:2], by = "variable")
chicago_income$label <- as_factor(str_replace(chicago_income$label, ".*!!(.*)", "\\1"))
chicago_income <- left_join(chicago_income, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_income) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#There are two outliers estimate over 4000. Let's examine to see what zip code this is.
arrange(chicago_income, desc(estimate))

#60614, 60657 are outliers

#ZIP 60604 is an outlier. We will throw out this point
chicago_income_new <- chicago_income %>% filter(!GEOID %in% c("60614", "60657"))

ggplot(chicago_income_new) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
cor.test(chicago_income$estimate, chicago_income$case_rate, use = "complete.obs")
#With outliers, cor = 0.19
cor.test(chicago_income_new$estimate, chicago_income_new$case_rate, use = "complete.obs")
#Without outliers, cor = 0.22

chicago_income_six_figure <- chicago_income %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("$200,000 or more",
                      "$150,000 to $199,999",
                      "$125,000 to $149,999",
                      "$100,000 to $124,999")) %>% 
  mutate(prop_100K = sum(prop)) %>% 
  dplyr::select(GEOID, prop_100K) %>% 
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

chicago_income_six_figure_cor <- left_join(chicago_income_six_figure, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_income_six_figure_cor$prop_100K, chicago_income_six_figure_cor$case_rate, use = "complete.obs")


chicago_income_under_30K <- chicago_income %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("$25,000 to $29,999",
                      "$20,000 to $24,999",
                      "$15,000 to $19,999",
                      "$10,000 to $14,999",
                      "Less than $10,000")) %>% 
  mutate(prop_30K = sum(prop)) %>% 
  dplyr::select(GEOID, prop_30K) %>% 
  distinct(GEOID, prop_30K)

lvls <- chicago_income_under_30K %>% 
  arrange(prop_30K) %>% 
  pull(GEOID)

ggplot(chicago_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))



### Test Correlation between percentage of population making 6-figure
### incomes to the case rate of COVID

chicago_income_under_30K_cor <- left_join(chicago_income_under_30K, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_income_under_30K_cor$prop_30K, chicago_income_under_30K_cor$case_rate, use = "complete.obs")


#------------------------------------------------------------------------------------------
### Poverty Status in Past 12 Months by Age
chicago_pov <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B17020") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B17020_001"))
chicago_pov <- left_join(chicago_pov, variables_2018[, 1:2], by = "variable")
chicago_pov$label <- as_factor(str_replace(chicago_pov$label, ".*!!(.*)", "\\1"))

chicago_pov <- left_join(chicago_pov, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_pov) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#There is an outlier with an estimate close to 100000
arrange(chicago_pov, desc(estimate))
arrange(chicago_pov, desc(case_rate))

#60629 is an outlier

#ZIP 60604 is an outlier. We will throw out this point
chicago_pov_new <- chicago_pov %>% filter(!GEOID %in% c("60629"))

ggplot(chicago_pov_new) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
cor.test(chicago_pov$estimate, chicago_pov$case_rate, use = "complete.obs")
#With outlier, cor = 0.07
cor.test(chicago_pov_new$estimate, chicago_pov_new$case_rate, use = "complete.obs")
#Without outlier, cor = 0.06



ggplot(chicago_pov) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

chicago_below_pov_lvl <- chicago_pov %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Income in the past 12 months below poverty level")) %>% 
  mutate(prop_below_pov = sum(prop)) %>% 
  dplyr::select(GEOID, prop_below_pov) %>% 
  distinct(GEOID, prop_below_pov)

lvls <- chicago_below_pov_lvl %>% 
  arrange(prop_below_pov) %>% 
  pull(GEOID)

ggplot(chicago_pov) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population income in the past 12 months below poverty level
### to the case rate of COVID
chicago_pov_cor <- left_join(chicago_below_pov_lvl, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_pov_cor$prop_below_pov, chicago_pov_cor$case_rate, use = "complete.obs")

chicago_above_pov_lvl <- chicago_pov %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Income in the past 12 months at or above poverty level")) %>% 
  mutate(prop_above_pov = sum(prop)) %>% 
  dplyr::select(GEOID, prop_above_pov) %>% 
  distinct(GEOID, prop_above_pov)

lvls <- chicago_above_pov_lvl %>% 
  arrange(prop_above_pov) %>% 
  pull(GEOID)

ggplot(chicago_pov) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population income in the past 12 months below poverty level
### to the case rate of COVID
chicago_above_pov_cor <- left_join(chicago_above_pov_lvl, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_above_pov_cor$prop_above_pov, chicago_above_pov_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code
chicago_pov <- left_join(chicago_pov, chicago_covid, by = c("GEOID" = "Zip"))



#Analyzing above poverty level data
chicago_above_pov_lvl <- chicago_pov %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Income in the past 12 months at or above poverty level")) %>% 
  mutate(prop_above_pov = sum(prop)) %>% 
  dplyr::select(GEOID, prop_above_pov) %>% 
  distinct(GEOID, prop_above_pov)

lvls_abv_pov <- chicago_above_pov_lvl %>% 
  arrange(prop_above_pov) %>% 
  pull(GEOID)

chicago_pov <- left_join(chicago_pov, chicago_covid, by = c("GEOID" = "Zip"))

ggplot(chicago_pov) +
  geom_col(aes(factor(GEOID, levels = lvls_abv_pov), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Poverty Distribution for Each Chicago Zip Code",
       x = "Zip Code",
       y = "Proportion of Population", 
       fill = "Poverty Level")

#Analyzing below poverty data
chicago_pov_cor <- left_join(chicago_below_pov_lvl, chicago_covid, by = c("GEOID"="Zip"))

ggplot(chicago_pov_cor, aes(prop_below_pov, case_rate)) +
  geom_point() +
  labs(title = "Slight Positive Correlation Between Infection and Poverty",
       subtitle = "ZIP code 60604 is an outlier.",
       x = "Proportion of Population Below the Poverty Line",
       y = "Case Rate (per 100,000)")+
  ggrepel::geom_label_repel(data = filter(chicago_pov_cor, case_rate > .05),
                            aes(label = GEOID))

chicago_pov_cor_sans60604 <- chicago_pov_cor %>% 
  filter(case_rate < .05)
cor.test(chicago_pov_cor_sans60604$prop_below_pov, chicago_pov_cor_sans60604$case_rate, use = "complete.obs")

#Our scatterplot "fans" out (more spread out) as the proportion of the population below the poverty line increases. We see another p-value which is significant at 0.01462. We have a slight positive correlation between individuals below the poverty line and case rate of 0.33 when we exclude our outlier of ZIP code 60604.


#Now to look at those above the poverty line
chicago_above_pov_cor <- left_join(chicago_above_pov_lvl, chicago_covid, by = c("GEOID"="Zip"))

ggplot(chicago_above_pov_cor, aes(prop_above_pov, case_rate)) +
  geom_point() +
  labs(title = "Slight Positive Correlation Between Infection and Poverty",
       subtitle = "ZIP code 60604 is an outlier.",
       x = "Proportion of Population Above the Poverty Line",
       y = "Case Rate (per 100,000)")+
  ggrepel::geom_label_repel(data = filter(chicago_above_pov_cor, case_rate > .05),
                            aes(label = GEOID))

chicago_above_pov_cor_sans60604 <- chicago_above_pov_cor %>% 
  filter(case_rate < .05)
cor.test(chicago_above_pov_cor_sans60604$prop_above_pov, chicago_above_pov_cor_sans60604$case_rate, use = "complete.obs")

#We see a similar scatterplot of fanning. As the proportion of the population above the poverty line decreases, the case rate seems to be more spread out. A p-value is calculated to be 0.01462 and the correlation is -0.33.




#-----------------------------------------------------------------------------------------------
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

chicago_occ <- left_join(chicago_occ, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_occ) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
#No outliers
cor.test(chicago_occ$estimate, chicago_occ$case_rate, use = "complete.obs")
#Small correlation

chicago_occ_essential <- chicago_occ %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Natural resources, construction, and maintenance occupations",
                      "Production, transportation, and material moving occupations")) %>% 
  mutate(prop_essential = sum(prop)) %>% 
  dplyr::select(GEOID, prop_essential) %>% 
  distinct(GEOID, prop_essential)

lvls <- chicago_occ_essential %>% 
  arrange(prop_essential) %>% 
  pull(GEOID)

ggplot(chicago_occ) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population that have essential occupations
### to the case rate of COVID
chicago_occ_cor <- left_join(chicago_occ_essential, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_occ_cor$prop_essential, chicago_occ_cor$case_rate, use = "complete.obs")

#Nonessential occupations analysis
chicago_occ_nonessential <- chicago_occ %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Management, business, science, and arts occupations",
                      "Service occupations",
                      "Sales and office occupations",
                      "Employee of private company workers")) %>% 
  mutate(prop_nonessential = sum(prop)) %>% 
  dplyr::select(GEOID, prop_nonessential) %>% 
  distinct(GEOID, prop_nonessential)

lvls <- chicago_occ_nonessential %>% 
  arrange(prop_nonessential) %>% 
  pull(GEOID)

ggplot(chicago_occ) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population that have nonessential occupations
### to the case rate of COVID
chicago_occ_nonessential_cor <- left_join(chicago_occ_nonessential, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_occ_nonessential_cor$prop_nonessential, chicago_occ_nonessential_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code
chicago_occ <- left_join(chicago_occ, chicago_covid, by = c("GEOID" = "Zip"))

#-----------------------------------------------------------------------------------------------
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

chicago_sex <- left_join(chicago_sex, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_sex) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
#No outliers
cor.test(chicago_sex$estimate, chicago_sex$case_rate, use = "complete.obs")
#cor = 0.05

chicago_over_sixty <- chicago_sex %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("60 and 61 years",
                      "62 to 64 years",
                      "65 and 66 years",
                      "67 to 69 years",
                      "70 to 74 years",
                      "75 to 79 years",
                      "80 to 84 years",
                      "85 years and over")) %>% 
  mutate(prop_60 = sum(prop)) %>% 
  dplyr::select(GEOID, prop_60) %>% 
  distinct(GEOID, prop_60)

lvls <- chicago_over_sixty %>% 
  arrange(prop_60) %>% 
  pull(GEOID)

ggplot(chicago_sex) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population over 60
### to the case rate of COVID
chicago_sex_cor <- left_join(chicago_over_sixty, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_sex_cor$prop_60, chicago_sex_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code
chicago_sex <- left_join(chicago_sex, chicago_covid, by = c("GEOID" = "Zip"))

#-----------------------------------------------------------------------------------------------------
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
chicago_race <- left_join(chicago_race, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_race) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
#No outliers

cor.test(chicago_race$estimate, chicago_race$case_rate, use = "complete.obs")
#cor = 0.06

chicago_black <- chicago_race %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Black or African American alone")) %>% 
  mutate(prop_black = sum(prop)) %>% 
  dplyr::select(GEOID, prop_black) %>% 
  distinct(GEOID, prop_black)

lvls <- chicago_black %>% 
  arrange(prop_black) %>% 
  pull(GEOID)

ggplot(chicago_race) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population that is black
### to the case rate of COVID
chicago_race_cor <- left_join(chicago_black, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_race_cor$prop_black, chicago_race_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code
chicago_race <- left_join(chicago_race, chicago_covid, by = c("GEOID" = "Zip"))

#---------------------------------------------------------------------------------------------------------
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

chicago_citizen <- left_join(chicago_citizen, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_citizen) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#There is an outlier with with estimate close to 80000. Let's examine to see what zip code this is.
arrange(chicago_citizen, desc(estimate))

#60629 is an outlier

#ZIP 60629 is an outlier. We will throw out this point
chicago_citizen_new <- chicago_citizen %>% filter(!GEOID %in% c("60629"))

ggplot(chicago_citizen_new) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
cor.test(chicago_citizen$estimate, chicago_citizen$case_rate, use = "complete.obs")
#With outlier, cor = 0.08
cor.test(chicago_citizen_new$estimate, chicago_citizen_new$case_rate, use = "complete.obs")
#Without outlier, cor = 0.06

chicago_not_citizen <- chicago_citizen %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Not a U.S. citizen")) %>% 
  mutate(prop_not_citizen = sum(prop)) %>% 
  dplyr::select(GEOID, prop_not_citizen) %>% 
  distinct(GEOID, prop_not_citizen)

lvls <- chicago_not_citizen %>% 
  arrange(prop_not_citizen) %>% 
  pull(GEOID)

ggplot(chicago_citizen) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population that are not citizens
### to the case rate of COVID
chicago_citizen_cor <- left_join(chicago_not_citizen, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_citizen_cor$prop_not_citizen, chicago_citizen_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code
chicago_citizen <- left_join(chicago_citizen, chicago_covid, by = c("GEOID" = "Zip"))

#-----------------------------------------------------------------------------------------------------
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
chicago_trans <- left_join(chicago_trans, chicago_covid, by = c("GEOID" = "Zip"))

#Scatterplots
ggplot(chicago_trans) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
#No outlier

cor.test(chicago_trans$estimate, chicago_trans$case_rate, use = "complete.obs")
#With outliers, cor = 0.03


chicago_public_trans <- chicago_trans %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Public transportation (excluding taxicab)",
                      "Bus or trolley bus",
                      "Streetcar or trolley car (carro publico in Puerto Rico)",
                      "Subway or elevated",
                      "Railroad")) %>% 
  mutate(prop_public = sum(prop)) %>% 
  dplyr::select(GEOID, prop_public) %>% 
  distinct(GEOID, prop_public)

lvls <- chicago_public_trans %>% 
  arrange(prop_public) %>% 
  pull(GEOID)

ggplot(chicago_trans) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population using public transportation
### to the case rate of COVID
chicago_trans_cor <- left_join(chicago_public_trans, chicago_covid, by = c("GEOID"="Zip"))
cor.test(chicago_trans_cor$prop_public, chicago_trans_cor$case_rate, use = "complete.obs")

## Plot COVID data for zip code
chicago_income <- left_join(chicago_income, chicago_covid, by = c("GEOID" = "Zip"))


#-------------------------------------------------------------------------------------------------------------
### Gini Index
chicago_gini <- get_acs(geography = "zcta",
                         year = 2018,
                         table = "B19083") %>% 
  filter(GEOID %in% zip_code_chicago$zip)
chicago_gini <- left_join(chicago_gini, variables_2018[, 1:2], by = "variable")
chicago_gini$label <- as_factor(str_replace(chicago_gini$label, ".*!!(.*)", "\\1"))
chicago_gini <- left_join(chicago_gini, chicago_covid, by = c("GEOID" = "Zip"))


ggplot(chicago_gini) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#There is an outlier with with a case rate close to 0.06. Let's examine to see what zip code this is.
arrange(chicago_gini, desc(case_rate))

#ZIP 60604 is an outlier. We will throw out this point
chicago_gini <- chicago_gini %>% filter(!GEOID %in% c("60604"))

ggplot(chicago_gini) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


### Test Correlation between percentage of population using public transportation
### to the case rate of COVID
cor.test(chicago_gini$estimate, chicago_gini$case_rate, use = "complete.obs")

#Before removing the outlier, our correlation was -0.27. Since we have removed it,
#our correlation is now -0.41








ggplot(chicago_gini) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#There is an outlier with with a case rate close to 0.06. Let's examine to see what zip code this is.
arrange(chicago_gini, desc(case_rate))

#ZIP 60604 is an outlier. We will throw out this point
chicago_gini <- chicago_gini %>% filter(!GEOID %in% c("60604"))

ggplot(chicago_gini) +
  geom_point(aes(x = estimate, y = case_rate))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))




