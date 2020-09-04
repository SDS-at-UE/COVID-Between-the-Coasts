# louisville_eda.R
## EDA on zip code data comparisons for Louisville, KY.
## Goal is to identify zip codes with differing characteristics
## and compare their COVID numbers

source("Scripts/functions.R")

######################################
# Retrieve zip code to county dataset
######################################

zip_code <- read_csv("Data/zip_county.csv")
zip_code_louis <- zip_code %>% filter(primary_city == "Louisville",
                                      state == "KY")

######################################
# Retrieve Income data
######################################

louis_income <- get_acs(geography = "zcta",
                        table = "B19101") %>% 
  filter(GEOID %in% zip_code_louis$zip,
         !variable %in% c("B19101_001"))

louis_income <- left_join(louis_income, variables_2018[, 1:2], by = "variable")

louis_income$label <- as_factor(str_replace(louis_income$label, ".*!!(.*)", "\\1"))

# Graph income data to see differences in zip codes

## attempt to order the x-axis of the graph
levels_income <- names(sort(tapply(filter(louis_income, label == "$200,000 or more")$estimate, louis_income$GEOID, mean)))

ggplot(louis_income) +
  geom_col(aes(GEOID, estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
