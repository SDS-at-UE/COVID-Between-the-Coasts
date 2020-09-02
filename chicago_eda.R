# Chicago EDA
## Exploring demographic data for Chicago

library(tidycensus)
library(tidyverse)

### Family Income in the Past 12 Months
chicago_income <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B19101")
chicago_income <- left_join(chicago_income, variables_2018[, 1:2], by = "variable")
chicago_income <- filter(chicago_income, between(chicago_income$GEOID, 60007, 60803))

### Poverty Status in Past 12 Months by Age
chicago_pov <- get_acs(geography = "zcta",
                       year = 2018,
                       table = "B17020")
chicago_pov <- left_join(chicago_pov, variables_2018[, 1:2], by ="variable")
chicago_pov <- filter(chicago_pov, between(chicago_pov$GEOID, 60007, 60803))

### Occupation by Class of Worker for the Civilian Employed Population 16 Years and Over
chicago_occ <- get_acs(geography = "zcta",
                       year = 2018,
                       table = "C24060")
chicago_occ <- left_join(chicago_occ, variables_2018[, 1:2], by ="variable")
chicago_occ <- filter(chicago_occ, between(chicago_occ$GEOID, 60007, 60803))

### Sex By Age
chicago_sex <- get_acs(geography = "zcta",
                       year = 2018,
                       table = "B01001")
chicago_sex <- left_join(chicago_sex, variables_2018[, 1:2], by ="variable")
chicago_sex <- filter(chicago_sex, between(chicago_sex$GEOID, 60007, 60803))

### Race
chicago_race <- get_acs(geography = "zcta",
                       year = 2018,
                       table = "B02001")
chicago_race <- left_join(chicago_race, variables_2018[, 1:2], by ="variable")
chicago_race <- filter(chicago_race, between(chicago_race$GEOID, 60007, 60803))

### Nativity and Citizenship Status in the United States
chicago_citizen <- get_acs(geography = "zcta",
                        year = 2018,
                        table = "B05001")
chicago_citizen <- left_join(chicago_citizen, variables_2018[, 1:2], by ="variable")
chicago_citizen <- filter(chicago_citizen, between(chicago_citizen$GEOID, 60007, 60803))

### MEANS OF TRANSPORTATION TO WORK
chicago_trans <- get_acs(geography = "zcta",
                           year = 2018,
                           table = "B08301")
chicago_trans <- left_join(chicago_trans, variables_2018[, 1:2], by ="variable")
chicago_trans <- filter(chicago_trans, between(chicago_trans$GEOID, 60007, 60803))


