# Minneapolis EDA
## Exploring demographic data for Minneapolis

library(tidycensus)
library(tidyverse)

# Minneapolis ALLOCATION OF OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER
minneapolis_employment <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B99242")
minneapolis_employment <- left_join(minneapolis_data, variables_2018[, 1:2], by = "variable")
minneapolis_employment <- filter(minneapolis_data, minneapolis_data$GEOID == "55434" | minneapolis_data$GEOID == "55448")

# Minneapolis AGE BY DISABILITY STATUS BY HEALTH INSURANCE COVERAGE STATUS
minneapolis_insurance <- get_acs(geography = "zcta",
                                 year = 2018,
                                 table = "B18135")
minneapolis_insurance <- left_join(minneapolis_data, variables_2018[, 1:2], by = "variable")
minneapolis_insurance <- filter(minneapolis_data, minneapolis_data$GEOID == "55434" | minneapolis_data$GEOID == "55448")

# Minneapolis PUBLIC ASSISTANCE INCOME OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS FOR HOUSEHOLDS
minneapolis_stamps <- get_acs(geography = "zcta",
                                 year = 2018,
                                 table = "B19058")
minneapolis_stamps <- left_join(minneapolis_data, variables_2018[, 1:2], by = "variable")
minneapolis_stamps <- filter(minneapolis_data, minneapolis_data$GEOID == "55434" | minneapolis_data$GEOID == "55448")

# Minneapolis MEANS OF TRANSPORTATION TO WORK
minneapolis_transportation <- get_acs(geography = "zcta",
                              year = 2018,
                              table = "B08301")
minneapolis_transportation <- left_join(minneapolis_data, variables_2018[, 1:2], by = "variable")
minneapolis_transportation <- filter(minneapolis_data, minneapolis_data$GEOID == "55434" | minneapolis_data$GEOID == "55448")
