# Chicago EDA
## Exploring demographic data for Chicago

library(tidycensus)
library(tidyverse)

### Chicago zip codes
zip_code <- read_csv("zip_county.csv")
zip_code_chicago <- zip_code %>% filter(primary_city == "Chicago",
                                        state == "IL")


### Family Income in the Past 12 Months
chicago_income <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B19101") %>% 
                  filter(GEOID %in% zip_code_chicago$zip,
                         !variable %in% c("B19101_001"))
chicago_income <- left_join(chicago_income, variables_2018[, 1:2], by = "variable")
chicago_income$label <- as_factor(str_replace(chicago_income$label, ".*!!(.*)", "\\1"))

ggplot(chicago_income) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Poverty Status in Past 12 Months by Age
chicago_pov <- get_acs(geography = "zcta",
                          year = 2018,
                          table = "B17020") %>% 
  filter(GEOID %in% zip_code_chicago$zip,
         !variable %in% c("B17020_001"))
chicago_pov <- left_join(chicago_pov, variables_2018[, 1:2], by = "variable")
chicago_pov$label <- as_factor(str_replace(chicago_pov$label, ".*!!(.*)", "\\1"))

ggplot(chicago_pov) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


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

