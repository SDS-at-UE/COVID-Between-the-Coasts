# Minneapolis EDA
## Exploring demographic data for Minneapolis
#########################################################################################################
knitr::opts_chunk$set(echo = TRUE)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(stringr)
library(sf)
library(tigris)
source("../Scripts/functions.R")
options(tigris_use_cache = TRUE)
variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>%
  rename(variable = name)
zip_code <- read_csv("../Data/zip_county.csv")
zip_code_minn <- zip_code %>% filter(primary_city == "Minneapolis",
                                     state == "MN")
minn_covid <- read_csv("../Data/minnesota_covid_data.csv",
                       col_types = cols(ZIP = col_character())) %>%
  filter(ZIP %in% zip_code_minn$zip)
minn_covid$Cases <- if_else(minn_covid$Cases == "<=5", 5, as.numeric(minn_covid$Cases))
minn_population <- get_acs(geography = "zcta",
                           variable = "B01003_001") %>%
  filter(GEOID %in% zip_code_minn$zip)
minneapolis_covid <- left_join(minn_population, minn_covid, by = c("GEOID" = "ZIP"))
minneapolis_covid <- minneapolis_covid %>% 
  rename(ZIP = GEOID,
         Population = estimate) %>%
  mutate(case_rate = Cases/Population)
minneapolis_covid <- select(minneapolis_covid, ZIP, Population, Cases, case_rate)
###############################################################################################################

# Essential and Nonessential Occupations 
minn_occupation <- get_acs(geography = "zcta",
                           table = "C24060") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("C24060_001"))
minn_occupation <- left_join(minn_occupation, variables_2018[, 1:2], by = "variable")
minn_occupation$label <- as_factor(str_replace(minn_occupation$label, ".*!!(.*)", "\\1"))

# Analyzing Essential Occupations
minn_occupation_essen <- minn_occupation %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Natural resources, construction, and maintenance occupations",
                      "Production, transportation, and material moving occupations")) %>% 
  mutate(prop_essen = sum(prop)) %>% 
  select(GEOID, prop_essen) %>% 
  distinct(GEOID, prop_essen)

lvls <- minn_occupation_essen %>% 
  arrange(prop_essen) %>% 
  pull(GEOID)

minn_occupation <- left_join(minn_occupation, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_occupation) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Occupations for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Occupation")

# Scatter plot for Essential
ggplot(minn_occupation) +
  geom_point(aes(x = estimate, y = case_rate.x)) +
  labs(title = "Minneapolis Essential",
       x = "Population",
       y = "Case Rate")

#Test correlation between population with essential jobs to case rate of COVID. 
minn_occupation_essen_cor <- left_join(minn_occupation_essen, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_occupation_essen_cor$prop_essen, minn_occupation_essen_cor$case_rate, use = "complete.obs")

# Analyzing Nonessential Occupations
minn_occupation_nonessen <- minn_occupation %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Management, business, science, and arts occupations",
                      "Service occupations",
                      "Sales and office occupations",
                      "Employee of private company workers")) %>% 
  mutate(prop_nonessen = sum(prop)) %>% 
  select(GEOID, prop_nonessen) %>% 
  distinct(GEOID, prop_nonessen)

lvls <- minn_occupation_nonessen %>% 
  arrange(prop_nonessen) %>% 
  pull(GEOID)

minn_occupation <- left_join(minn_occupation, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_occupation) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Occupations for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Occupation")

# Scatterplot Nonessential
ggplot(minn_occupation) +
  geom_point(aes(x = estimate, y = case_rate)) +
  labs(title = "Minneapolis Nonessential",
       x = "Population",
       y = "Case Rate")

# Test correlation between population with nonessential jobs to the case rate of COVID
minn_occupation_nonessen_cor <- left_join(minn_occupation_nonessen, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_occupation_nonessen_cor$prop_nonessen, minn_occupation_nonessen_cor$case_rate, use = "complete.obs")

########################################################################################################################

# Citizenship
minn_citi <- get_acs(geography = "zcta",
                     table = "B05001") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B05001_001"))
minn_citi <- left_join(minn_citi, variables_2018[, 1:2], by = "variable")
minn_citi$label <- as_factor(str_replace(minn_citi$label, ".*!!(.*)", "\\1"))

# Analyze citizenship
minn_citi_status <- minn_citi %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Not a U.S. citizen")) %>% 
  mutate(prop_not_citizen = sum(prop)) %>% 
  select(GEOID, prop_not_citizen) %>% 
  distinct(GEOID, prop_not_citizen)

lvls <- minn_citi_status %>% 
  arrange(prop_not_citizen) %>% 
  pull(GEOID)

minn_citi <- left_join(minn_citi, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_citi) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Citizenship Status for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Citizenship Status")

# Scatterplot citizenship
ggplot(minn_citi) +
  geom_point(aes(x = estimate, y = case_rate)) +
  labs(title = "Minneapolis Citizenship",
       x = "Population",
       y = "Case Rate")

# Test correlation between precentage of population that is not a U.S. citizen to case rate.
minn_citi_cor <- left_join(minn_citi_status, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_citi_cor$prop_not_citizen, minn_citi_cor$case_rate, use = "complete.obs")

##############################################################################################################################

# Income 100K+ VS 30K-

minn_income <- get_acs(geography = "zcta",
                       table = "B19101") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B19101_001"))
minn_income <- left_join(minn_income, variables_2018[, 1:2], by = "variable")
minn_income$label <- as_factor(str_replace(minn_income$label, ".*!!(.*)", "\\1"))

# Analyzing 100K and over income
minn_income__sixfig <- minn_income %>% 
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

lvls <- minn_income__sixfig %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

minn_income <- left_join(minn_income, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Income Status in past year per household for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Income Status")

# Income 100k+
ggplot(minn_occupation) +
  geom_point(aes(x = estimate, y = case_rate.x)) +
  labs(title = "Minneapolis Income 100k+",
       x = "Population",
       y = "Case Rate")

# Test correlation between percent of population making six figure incomes to case rate of COVID
minn_income_sixfig_cor <- left_join(minn_income__sixfig, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_income_sixfig_cor$prop_100K, minn_income_sixfig_cor$case_rate, use = "complete.obs")

# Analyzing 30K and under income
minn_income_30K_below <- minn_income %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("$25,000 to $29,999",
                      "$20,000 to $24,999",
                      "$15,000 to $19,999",
                      "$10,000 to $14,999",
                      "Less than $10,000")) %>% 
  mutate(prop_30K = sum(prop)) %>% 
  select(GEOID, prop_30K) %>% 
  distinct(GEOID, prop_30K)

lvls <- minn_income_30K_below %>% 
  arrange(prop_30K) %>% 
  pull(GEOID)

minn_income <- left_join(minn_income, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "Income Status in past year per household for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Income Status")

# Income 30k below
ggplot(minn_occupation) +
  geom_point(aes(x = estimate, y = case_rate.x)) +
  labs(title = "Minneapolis Income 30k and below",
       x = "Population",
       y = "Case Rate")

# Test correlation between percent of population making 30K and under incomes to case rate of COVID
minn_income_30K_below_cor <- left_join(minn_income_30K_below, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_income_30K_below_cor$prop_30K, minn_income_30K_below_cor$case_rate, use = "complete.obs")
#######################################################################################################################################

# Race 
minn_race <- get_acs(geography = "zcta",
                     table = "B02001") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B02001_001"))
minn_race <- left_join(minn_race, variables_2018[, 1:2], by = "variable")
minn_race$label <- as_factor(str_replace(minn_race$label, ".*!!(.*)", "\\1"))

# Analyzing Afican Americans in Minneapolis
minn_AA <- minn_race %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Black or African American alone")) %>% 
  mutate(prop_AA = sum(prop)) %>% 
  select(GEOID, prop_AA) %>% 
  distinct(GEOID, prop_AA)

lvls <- minn_AA %>% 
  arrange(prop_AA) %>% 
  pull(GEOID)

minn_race <- left_join(minn_race, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_race) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Racial Status for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Racial Status")

# Scatterplot race
ggplot(minn_race) +
  geom_point(aes(x = estimate, y = case_rate)) +
  labs(title = "Minneapolis Race",
       x = "Population", 
       y = "Case rate")

# Test correlation between percentage of population that is African American and case rate of COVID
minn_race_cor <- left_join(minn_AA, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_race_cor$prop_AA, minn_race_cor$case_rate, use = "complete.obs")
#######################################################################################################################################

# Gini Index
minn_gini <- get_acs(geography = "zcta",
                     table = "B19083") %>% 
  filter(GEOID %in% zip_code_minn$zip)
minn_gini <- left_join(minn_gini, variables_2018[, 1:2], by = "variable")
minn_gini$label <- as_factor(str_replace(minn_gini$label, ".*!!(.*)", "\\1"))

# Analyze Gini Index for Minneapolis
minn_gini <- left_join(minn_gini, minneapolis_covid, by = c("GEOID" = "ZIP"))
minn_gini <- filter(minn_gini, GEOID != '55450')

# Scatterplot Gini
ggplot(minn_gini) +
  geom_point(aes(x = estimate, y = case_rate)) +
  labs(title = "Gini Index for Each Minneapolis Zip Code",
       x = "Gini Index",
       y = "Case Rate")

# Test correlation between gini index and case rate of COVID
cor.test(minn_gini$estimate, minn_gini$case_rate, use = "complete.obs")
######################################################################################################################################

# Insurance 
minn_insurance <- get_acs(geography = "zcta",
                          table = "B18135") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B18135_023"))
minn_insurance <- left_join(minn_insurance, variables_2018[, 1:2], by = "variable")
minn_insurance$label <- as_factor(str_replace(minn_insurance$label, ".*!!(.*)", "\\1"))

# Analyze private health insurance coverage
minn_insurance_private <- minn_insurance %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("With private health insurance coverage")) %>% 
  mutate(prop_priv = sum(prop)) %>% 
  select(GEOID, prop_priv) %>% 
  distinct(GEOID, prop_priv)

lvls <- minn_insurance_private %>% 
  arrange(prop_priv) %>% 
  pull(GEOID)

minn_insurance <- left_join(minn_insurance, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_insurance) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Insurance Coverage for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Insurance Coverage")

# Scatterplot private
ggplot(minn_insurance) +
  geom_point(aes(x = estimate, y = case_rate.x)) +
  labs(title = "Minneapolis Private Insurance",
       x = "Population", 
       y = "Case rate")

# Test correlation between percentage of population with private health insurance and case rate of COVID
minn_private_cor <- left_join(minn_insurance_private, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_private_cor$prop_priv, minn_private_cor$case_rate, use = "complete.obs")

# Analyze public health insurance coverage
minn_insurance_public <- minn_insurance %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("With public health coverage")) %>% 
  mutate(prop_public = sum(prop)) %>% 
  select(GEOID, prop_public) %>% 
  distinct(GEOID, prop_public)

lvls <- minn_insurance_public %>% 
  arrange(prop_public) %>% 
  pull(GEOID)

minn_insurance <- left_join(minn_insurance, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_insurance) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Insurance Coverage for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Insurance Coverage")

# Scatterplot public 
ggplot(minn_insurance) +
  geom_point(aes(x = estimate, y = case_rate.x)) +
  labs(title = "Minneapolis Public Insurance",
       x = "Population", 
       y = "Case rate")

# Test correlation between percentage of population with public health insurance and case rate of COVID
minn_public_cor <- left_join(minn_insurance_public, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_public_cor$prop_public, minn_public_cor$case_rate, use = "complete.obs")
##############################################################################################################################

# Public Assistance 
minn_stamps <- get_acs(geography = "zcta",
                       table = "B19058") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B19058_002"))
minn_stamps <- left_join(minn_stamps, variables_2018[, 1:2], by = "variable")
minn_stamps$label <- as_factor(str_replace(minn_stamps$label, ".*!!(.*)", "\\1"))

# Analyzing families that accepted public assistance income in Minneapolis
minn_stamps_with <- minn_stamps %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Total")) %>% 
  mutate(prop_stamps = sum(prop)) %>% 
  select(GEOID, prop_stamps) %>% 
  distinct(GEOID, prop_stamps)

lvls <- minn_stamps_with %>% 
  arrange(prop_stamps) %>% 
  pull(GEOID)

minn_stamps <- left_join(minn_stamps, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_stamps) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Public Assistance Distribution for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Public Assistance")

# Scatterplot public assistance 
ggplot(minn_stamps) +
  geom_point(aes(x = estimate, y = case_rate)) +
  labs(title = "Minneapolis Public Assistance",
       x = "Population", 
       y = "Case rate")

# Test correlation between percentage of population that accepted public assistance and case rate of COVID
minn_stamps_cor <- left_join(minn_stamps_with, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_stamps_cor$prop_stamps, minn_stamps_cor$case_rate, use = "complete.obs")
###############################################################################################################################

# Public Transportation
minn_transport <- get_acs(geography = "zcta",
                          table = "B08301") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B08301_010"))
minn_transport <- left_join(minn_transport, variables_2018[, 1:2], by = "variable")
minn_transport$label <- as_factor(str_replace(minn_transport$label, ".*!!(.*)", "\\1"))

# Analyzing transportation
minn_transport_public <- minn_transport %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("Public transportation (excluding taxicab)",
                      "Bus or trolley bus",
                      "Streetcar or trolley car (carro publico in Puerto Rico)",
                      "Subway or elevated",
                      "Railroad")) %>% 
  mutate(prop_pub = sum(prop)) %>% 
  select(GEOID, prop_pub) %>% 
  distinct(GEOID, prop_pub)

lvls <- minn_transport_public %>% 
  arrange(prop_pub) %>% 
  pull(GEOID)

minn_transport <- left_join(minn_transport, minneapolis_covid, by = c("GEOID" = "ZIP"))

ggplot(minn_transport) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Transportation Distribution for Each Minneapolis Zip Code",
       x = "Zip Code",
       y = "Proportion", 
       fill = "Transportation")

# Scatterplot transportation
ggplot(minn_transport) +
  geom_point(aes(x = estimate, y = case_rate)) +
  labs(title = "Minneapolis Public Transportation",
       x = "Population", 
       y = "Case rate")

# Test the correlation between the percentage of the population using public transportation and case rate of COVID
minn_trans_cor <- left_join(minn_transport_public, minneapolis_covid, by = c("GEOID"="ZIP"))
cor.test(minn_trans_cor$prop_pub, minn_trans_cor$case_rate, use = "complete.obs")
