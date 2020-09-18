# Minneapolis EDA
## Exploring demographic data for Minneapolis

library(tidycensus)
library(tidyverse)

# Retrieve zip code to county dataset
zip_code <- read_csv("Data/zip_county.csv")
zip_code_minn <- zip_code %>% filter(primary_city == "Minneapolis",
                                      state == "MN")


# Minneapolis ALLOCATION OF OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER
minn_employment <- get_acs(geography = "zcta",
                        table = "B99242") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B99242_002"))

minn_employment <- left_join(minn_employment, variables_2018[, 1:2], by = "variable")

minn_employment$label <- as_factor(str_replace(minn_employment$label, ".*!!(.*)", "\\1"))

levels_income <- names(sort(tapply(filter(minn_employment, label == "Employed Population 16+")$estimate, minn_employment$GEOID, mean)))

ggplot(minn_employment) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# Minneapolis AGE BY DISABILITY STATUS BY HEALTH INSURANCE COVERAGE STATUS
minn_insurance <- get_acs(geography = "zcta",
                           table = "B18135") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B18135_023"))

minn_insurance <- left_join(minn_insurance, variables_2018[, 1:2], by = "variable")

minn_insurance$label <- as_factor(str_replace(minn_insurance$label, ".*!!(.*)", "\\1"))

levels_income <- names(sort(tapply(filter(minn_insurance, label == "19-64 with no health insurance or disability")$estimate, minn_insurance$GEOID, mean)))

ggplot(minn_insurance) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# Minneapolis PUBLIC ASSISTANCE INCOME OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS FOR HOUSEHOLDS
minn_stamps <- get_acs(geography = "zcta",
                          table = "B19058") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B19058_002"))

minn_stamps <- left_join(minn_stamps, variables_2018[, 1:2], by = "variable")

minn_stamps$label <- as_factor(str_replace(minn_stamps$label, ".*!!(.*)", "\\1"))

levels_income <- names(sort(tapply(filter(minn_stamps, label == "Public assistance or food stamps past 12 months")$estimate, minn_stamps$GEOID, mean)))

ggplot(minn_stamps) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# Minneapolis MEANS OF TRANSPORTATION TO WORK
minn_transport <- get_acs(geography = "zcta",
                       table = "B08301") %>% 
  filter(GEOID %in% zip_code_minn$zip,
         !variable %in% c("B08301_010"))

minn_transport <- left_join(minn_transport, variables_2018[, 1:2], by = "variable")

minn_transport$label <- as_factor(str_replace(minn_transport$label, ".*!!(.*)", "\\1"))

levels_income <- names(sort(tapply(filter(minn_transport, label == "Taking public transportation to work (no taxi)")$estimate, minn_transport$GEOID, mean)))

ggplot(minn_transport) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
