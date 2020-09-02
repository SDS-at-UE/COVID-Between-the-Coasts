# Chicago EDA
## Exploring demographic data for Chicago

library(tidycensus)
library(tidyverse)

chicago_data <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B27019",
                        geometry = TRUE)
chicago_data <- left_join(chicago_data, variables_2018[, 1:2], by = "variable")
