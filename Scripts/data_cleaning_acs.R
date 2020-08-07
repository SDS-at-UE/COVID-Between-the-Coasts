# Data Cleaning of ACS Files

library(tidyverse)
library(tidycensus)

## Run/store all functions from functions.R script
source("Scripts/functions.R")


# census_api_key("7cf0c318e343f70900ce428bc2646b7f776807e5",
#               install = TRUE)

# Do not need to run next line all of the time. Since it's
# cached, it should already be loaded whenever you start R.
# Just run once per computer.
#variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>% 
#  rename(variable = name)

states_of_interest <- c("IN","IL","KY", "OH", "MI", "MN", "WI")

######################################
# Retrieve Income data
######################################

income <- get_data(states_of_interest, "B19101") %>% 
  filter(!variable %in% c("B19101_001"))

income$label <- as_factor(str_replace(income$label, ".*!!(.*)", "\\1"))

write_csv(income, "Data/income.csv")


######################################
# Retrieve Sex, Age data
# Health insurance coverage also obtained
######################################

sex_age <- get_data(states_of_interest, "B27001")

sex_age <- sex_age %>% filter(str_count(label, "!!") >= 4)
  
sex_age$label <- str_remove(sex_age$label, "Estimate!!Total!!")

sex_age <- separate(sex_age,
                    label,
                    sep = "!!",
                    into = c("Sex", "Age", "HI_Coverage"))

sex_age$HI_Coverage <- if_else(sex_age$HI_Coverage == "No health insurance coverage", "No", "Yes")

sex_age$Sex <- as_factor(sex_age$Sex)
sex_age$Age <- as_factor(sex_age$Age)
sex_age$HI_Coverage <- as_factor(sex_age$HI_Coverage)  

write_csv(sex_age, "Data/sex_age.csv")


######################################
# Retrieve Type of Health Insurance data
######################################

health_private <- get_data(states_of_interest, "B27002")
health_public <- get_data(states_of_interest, "B27003")

health_private <- health_private %>% filter(str_count(label, "!!") >= 4)
health_public <- health_public %>% filter(str_count(label, "!!") >= 4)

health_private$label <- str_remove(health_private$label, "Estimate!!Total!!")
health_public$label <- str_remove(health_public$label, "Estimate!!Total!!")

health_private <- separate(health_private,
                           label,
                           sep = "!!",
                           into = c("Sex", "Age", "Private_HI"))
health_public <- separate(health_public,
                          label,
                          sep = "!!",
                          into = c("Sex", "Age", "Public_HI"))

health_private$Private_HI <- if_else(health_private$Private_HI == "No private health insurance", "No", "Yes")
health_public$Public_HI <- if_else(health_public$Public_HI == "No public coverage", "No", "Yes")

health_private$Sex <- as_factor(health_private$Sex)
health_public$Sex <- as_factor(health_public$Sex)
health_private$Age <- as_factor(health_private$Age)
health_public$Age <- as_factor(health_public$Age)
health_private$Private_HI <- as_factor(health_private$Private_HI)
health_public$Public_HI <- as_factor(health_public$Public_HI) 

write_csv(health_private, "Data/health_private.csv")
write_csv(health_public, "Data/health_public.csv")

######################################
# Retrieve Race data
######################################

race <- get_data(states_of_interest, "B02001")

race <- race %>% filter(str_count(label, "!!") == 2)

race$label <- as_factor(str_remove(race$label, "Estimate!!Total!!"))

write_csv(race, "Data/race.csv")


######################################
# Retrieve Education data
######################################

edu <- get_data(states_of_interest, "B15001")

edu <- edu %>% filter(str_count(label, "!!") == 4)

edu$label <- str_remove(edu$label, "Estimate!!Total!!")

edu <- separate(edu,
                label,
                sep = "!!",
                into = c("Sex", "Age", "Education"))

edu$Sex <- as_factor(edu$Sex)
edu$Age <- as_factor(edu$Age)
edu$Education <- as_factor(edu$Education)  

write_csv(edu, "Data/edu.csv")

######################################
# Retrieve Employment data
######################################

employ <- get_data(states_of_interest, "B23001")

employ <- employ %>% filter(str_count(label, "!!") >= 4,
                            str_count(label, "!!In labor force$") < 1,
                            str_count(label, "!!Civilian$") < 1)

employ$label <- str_remove(employ$label, "Estimate!!Total!!") 
employ$label <- str_remove(employ$label, "!!In labor force")
employ$label <- str_replace(employ$label, "Civilian!!Employed", "Employed Civilian")
employ$label <- str_replace(employ$label, "Civilian!!Unemployed", "Unemployed Civilian")

employ <- separate(employ,
                   label,
                   sep = "!!",
                   into = c("Sex", "Age", "Employment"))

write_csv(employ, "Data/employ.csv")

######################################
# Retrieve Ethnic data
######################################

ethnic <- get_data(states_of_interest, "B03002")

ethnic <- ethnic %>% filter(str_count(label, "!!") == 3)

ethnic$label <- str_remove(ethnic$label, "Estimate!!Total!!") 

ethnic <- separate(ethnic,
                   label,
                   sep = "!!",
                   into = c("Hispanic_Latino", "Race"))

ethnic$Hispanic_Latino <- if_else(ethnic$Hispanic_Latino == "Hispanic or Latino", "Yes", "No")

ethnic$Hispanic_Latino <- as_factor(ethnic$Hispanic_Latino)
ethnic$Race <- as_factor(ethnic$Race)

write_csv(ethnic, "Data/ethnic.csv")


######################################
# Retrieve Gini Index data (income inequality)
######################################

gini <- get_data(states_of_interest, "B19083")

gini$label <- str_remove(gini$label, "Estimate!!")

write_csv(gini, "Data/gini.csv")






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
