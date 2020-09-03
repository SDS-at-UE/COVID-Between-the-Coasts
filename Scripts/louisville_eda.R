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
# Retrieve COVID case data for Louisville
# collected from https://covid-19-in-jefferson-county-ky-lojic.hub.arcgis.com/
#####################################

louis_covid <- read_csv("Data/louisville_covid.csv",
                        col_types = cols(ZIP = col_character()))
louis_covid <- louis_covid %>% 
  mutate(case_rate = Cases/Population) %>% 
  rename(zip = ZIP,
         population = Population,
         cases = Cases)

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

## Order the x-axis of the graph based on income. 
### We group 6-figure incomes together and look
### at their proportion of the zip code and 
### sort in ascending order.
louis_income_six_figure <- louis_income %>% 
  group_by(GEOID) %>% 
  mutate(n = sum(estimate),
         prop = estimate/n) %>% 
  filter(label %in% c("$200,000 or more",
                      "$150,000 to $199,999",
                      "$125,000 to $149,999",
                      "$100,000 to $124,999")) %>% 
  mutate(prop_100K = sum(prop)) %>% 
  select(GEOID, prop_100K) %>% 
  distinct()

lvls <- louis_income_six_figure %>% 
  arrange(prop_100K) %>% 
  pull(GEOID)

ggplot(louis_income) +
  geom_col(aes(factor(GEOID, levels = lvls), estimate, fill = label),
           position = "fill") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

### Test Correlation between percentage of population making 6-figure
### incomes to the case rate of COVID

louis_income_cor <- left_join(louis_income_six_figure, louis_covid, by = c("GEOID"="zip"))
cor.test(louis_income_cor$prop_100K, louis_income_cor$case_rate, use = "complete.obs")
