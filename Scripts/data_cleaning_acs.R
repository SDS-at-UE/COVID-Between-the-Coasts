# Data Cleaning of ACS Files

## Run/store all functions from functions.R script
source("Scripts/functions.R")

library(tidyverse)
library(tidycensus)

census_api_key("7cf0c318e343f70900ce428bc2646b7f776807e5")

# Do not need to run next line all of the time. Since it's
# cached, it should already be loaded whenever you start R.
# Just run once per computer.
#variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>% 
#  rename(variable = name)


######################################
# Retrieve Income data
######################################

IN_income <- get_data("IN", "B06010") %>% 
  filter(variable %in% c("B06010_002",
                         str_c("B06010_00", 4:9),
                         str_c("B06010_0", 10:11)))

IN_income <- left_join(IN_income, variables_2018[,1:2], by = "variable")

IN_income$label <- as_factor(str_replace(IN_income$label, ".*!!(.*)", "\\1"))




######################################
# Retrieve Sex, Age data
# Health insurance coverage also obtained
######################################

IN_sex_age <- get_data("IN", "B27001")


######################################
# Retrieve Type of Health Insurance data
######################################

IN_health_private <- get_data("IN", "B27002")
IN_health_public <- get_data("IN", "B27003")


######################################
# Retrieve Race data
######################################

IN_race <- get_data("IN", "B02001")


######################################
# Retrieve Education data
######################################

IN_edu <- get_data("IN", "B15001")


######################################
# Retrieve Employment data
######################################

IN_employ <- get_data("IN", "B23001")


######################################
# Retrieve Ethnic data
######################################

IN_ethnic <- get_data("IN", "B03002")


















######################################
# Read in ACS Demographic/Housing Data
######################################
acs_demo <- read_csv("Data/ACS_Demographic_and_Housing_Estimates/ACSDP5Y2018.DP05_data_with_overlays_2020-07-23T014728.csv",
                     skip = 1,
                     na = c("*****","-"))

# Select only the relevant columns
acs_demo_1 <- acs_demo %>% select(id, `Geographic Area Name`, starts_with("Estimate")) 

acs_demo_2 <- acs_demo_1[, str_count(colnames(acs_demo_1), "!!") <= 4] %>% 
  select(-starts_with("Estimate!!Race alone"), 
         -ends_with("_1"),
         -contains("Two or more races!!"),
         -contains("Hispanic or Latino (of any race)!!"),
         -contains("Not Hispanic or Latino!!"),
         -contains("ratio")) %>% 
  select(-(36:45)) %>% 
  separate(`Geographic Area Name`, into = c("County", "State"), sep = ",")

# Drop columns with all NA
acs_demo_3 <- acs_demo_2[, !map_lgl(acs_demo_2, ~ all(is.na(.))), drop = FALSE]

#################################
# Read in ACS Education Data
#################################
acs_edu <- read_csv("Data/ACS_Educational_Attainment/ACSST5Y2018.S1501_data_with_overlays_2020-07-23T004821.csv",
                    skip = 1,
                    na = c("(X)","-"))

# Select only the relevant columns
acs_edu_1 <- acs_edu %>% select(id, `Geographic Area Name`, starts_with("Estimate"))

acs_edu_2 <- acs_edu_1 %>% select(-contains(c("Percent",
                                              "Total"))) %>% 
  separate(`Geographic Area Name`, into = c("County", "State"), sep = ",")

# Drop columns with all NA
acs_edu_3 <- acs_edu_2[, !map_lgl(acs_edu_2, ~ all(is.na(.))), drop = FALSE]

################################################
# Read in ACS Selected Economic Characteristics
################################################
acs_eco <- read_csv("Data/ACS_Selected_Economic_Characteristics/ACSDP5Y2018.DP03_data_with_overlays_2020-07-23T014119.csv",
                    skip = 1,
                    na = c("(X)"))

acs_eco_1 <- acs_eco %>% select(id, `Geographic Area Name`, starts_with("Estimate"))

acs_eco_2 <- acs_eco_1 %>% select(-contains(c("COMMUTING TO WORK",
                                              "!!Families",
                                              "!!Nonfamily households"))) %>% 
  separate(`Geographic Area Name`, into = c("County", "State"), sep = ",")

acs_eco_3 <- acs_eco_2[, !map_lgl(acs_eco_2, ~ all(is.na(.))), drop = FALSE]
