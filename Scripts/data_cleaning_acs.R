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

# This creates a reference database to convert between the 
# name of a state and its abbreviation
state_abb_to_name <- tibble(State = state.name, Abb = state.abb)


######################################
# Retrieve Income data
######################################

IN_income <- get_data("IN", "B19101") %>% 
  filter(!variable %in% c("B19101_001"))

IN_income$label <- as_factor(str_replace(IN_income$label, ".*!!(.*)", "\\1"))

write_csv(IN_income, "Data/IN_income.csv")


######################################
# Retrieve Sex, Age data
# Health insurance coverage also obtained
######################################

IN_sex_age <- get_data("IN", "B27001")

IN_sex_age <- IN_sex_age %>% filter(str_count(label, "!!") >= 4)
  
IN_sex_age$label <- str_remove(IN_sex_age$label, "Estimate!!Total!!")

IN_sex_age <- separate(IN_sex_age,
                       label,
                       sep = "!!",
                       into = c("Sex", "Age", "HI_Coverage"))

IN_sex_age$HI_Coverage <- if_else(IN_sex_age$HI_Coverage == "No health insurance coverage", "No", "Yes")

IN_sex_age$Sex <- as_factor(IN_sex_age$Sex)
IN_sex_age$Age <- as_factor(IN_sex_age$Age)
IN_sex_age$HI_Coverage <- as_factor(IN_sex_age$HI_Coverage)  

write_csv(IN_sex_age, "Data/IN_sex_age.csv")


######################################
# Retrieve Type of Health Insurance data
######################################

IN_health_private <- get_data("IN", "B27002")
IN_health_public <- get_data("IN", "B27003")

IN_health_private <- IN_health_private %>% filter(str_count(label, "!!") >= 4)
IN_health_public <- IN_health_public %>% filter(str_count(label, "!!") >= 4)

IN_health_private$label <- str_remove(IN_health_private$label, "Estimate!!Total!!")
IN_health_public$label <- str_remove(IN_health_public$label, "Estimate!!Total!!")

IN_health_private <- separate(IN_health_private,
                              label,
                              sep = "!!",
                              into = c("Sex", "Age", "Private_HI"))
IN_health_public <- separate(IN_health_public,
                             label,
                             sep = "!!",
                             into = c("Sex", "Age", "Public_HI"))

IN_health_private$Private_HI <- if_else(IN_health_private$Private_HI == "No private health insurance", "No", "Yes")
IN_health_public$Public_HI <- if_else(IN_health_public$Public_HI == "No public coverage", "No", "Yes")

IN_health_private$Sex <- as_factor(IN_health_private$Sex)
IN_health_public$Sex <- as_factor(IN_health_public$Sex)
IN_health_private$Age <- as_factor(IN_health_private$Age)
IN_health_public$Age <- as_factor(IN_health_public$Age)
IN_health_private$Private_HI <- as_factor(IN_health_private$Private_HI)
IN_health_public$Public_HI <- as_factor(IN_health_public$Public_HI) 

write_csv(IN_health_private, "Data/IN_health_private.csv")
write_csv(IN_health_public, "Data/IN_health_public.csv")

######################################
# Retrieve Race data
######################################

IN_race <- get_data("IN", "B02001")

IN_race <- IN_race %>% filter(str_count(label, "!!") == 2)

IN_race$label <- as_factor(str_remove(IN_race$label, "Estimate!!Total!!"))

write_csv(IN_race, "Data/IN_race.csv")


######################################
# Retrieve Education data
######################################

IN_edu <- get_data("IN", "B15001")

IN_edu <- IN_edu %>% filter(str_count(label, "!!") == 4)

IN_edu$label <- str_remove(IN_edu$label, "Estimate!!Total!!")

IN_edu <- separate(IN_edu,
                   label,
                   sep = "!!",
                   into = c("Sex", "Age", "Education"))

IN_edu$Sex <- as_factor(IN_edu$Sex)
IN_edu$Age <- as_factor(IN_edu$Age)
IN_edu$Education <- as_factor(IN_edu$Education)  

write_csv(IN_edu, "Data/IN_edu.csv")

######################################
# Retrieve Employment data
######################################

IN_employ <- get_data("IN", "B23001")

IN_employ <- IN_employ %>% filter(str_count(label, "!!") >= 4,
                                  str_count(label, "!!In labor force$") < 1,
                                  str_count(label, "!!Civilian$") < 1)

IN_employ$label <- str_remove(IN_employ$label, "Estimate!!Total!!") 
IN_employ$label <- str_remove(IN_employ$label, "!!In labor force")
IN_employ$label <- str_replace(IN_employ$label, "Civilian!!Employed", "Employed Civilian")
IN_employ$label <- str_replace(IN_employ$label, "Civilian!!Unemployed", "Unemployed Civilian")

IN_employ <- separate(IN_employ,
                      label,
                      sep = "!!",
                      into = c("Sex", "Age", "Employment"))

write_csv(IN_employ, "Data/IN_employ.csv")

######################################
# Retrieve Ethnic data
######################################

IN_ethnic <- get_data("IN", "B03002")

IN_ethnic <- IN_ethnic %>% filter(str_count(label, "!!") == 3)

IN_ethnic$label <- str_remove(IN_ethnic$label, "Estimate!!Total!!") 

IN_ethnic <- separate(IN_ethnic,
                      label,
                      sep = "!!",
                      into = c("Hispanic_Latino", "Race"))

IN_ethnic$Hispanic_Latino <- if_else(IN_ethnic$Hispanic_Latino == "Hispanic or Latino", "Yes", "No")

IN_ethnic$Hispanic_Latino <- as_factor(IN_ethnic$Hispanic_Latino)
IN_ethnic$Race <- as_factor(IN_ethnic$Race)

write_csv(IN_ethnic, "Data/IN_ethnic.csv")


######################################
# Retrieve Gini Index data (income inequality)
######################################

IN_gini <- get_data("IN", "B19083")

IN_gini$label <- str_remove(IN_gini$label, "Estimate!!")

write_csv(IN_gini, "Data/IN_gini.csv")


######################################
# Clean USA Facts Confirmed data
######################################

covid_conf <- read_csv("Data/usafacts_covid_confirmed.csv")

covid_conf <- covid_conf %>% select(`County Name`, State, tail(names(.),1)) %>% 
  rename_at(vars(tail(names(.),1)), funs(c("Cases")))

covid_conf <- left_join(covid_conf, state_abb_to_name, 
                             by = c("State" = "Abb")) %>% 
  select(!State) %>% 
  rename(State = State.y,
         County = `County Name`) %>% 
  mutate(NAME = str_c(County, State, sep = ", ")) %>% 
  select(NAME, Cases)

write_csv(covid_conf, "Data/covid_cases.csv")




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
