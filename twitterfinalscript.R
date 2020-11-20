install.packages("rtweet")
library(rtweet)
library(tidyverse)


twitter_token <- create_token(
  app = "WNINCovidBetweentheCoasts",
  consumer_key = "6WoyE8SKPd42xyX8cJA7PAAEo",
  consumer_secret = "vBeKwvD8AvsGlAvSUZyZSCuquuwBja7VowOg0KtNVLYGVptIsL",
  access_token = "1327467001850322944-tv4VeJdEAnMPZ8YnhxKDWr5FDeUzyM",
  access_secret = "4fp5a0ulTGF8Oc5EQR6rSCwkNEjzhiZS60iVHZC2O5FGg")


post_tweet("test")


covid_html_data <- read_html("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/") %>% 
  html_nodes('a') %>%
  html_attr('href') %>% 
  str_subset("\\.csv$")

# Extracting the data from the csv files 

cases <- read_csv(covid_html_data[1],
                  col_types = cols(
                    .default = col_character(),
                    `County Name` = col_character(),
                    State = col_character()
                  )) %>% 
  rename(county_name = `County Name`)
deaths <- read_csv(covid_html_data[2],
                   col_types = cols(
                     .default = col_character(),
                     `County Name` = col_character(),
                     State = col_character()
                   )) %>% 
  rename(county_name = `County Name`)
population <- read_csv(covid_html_data[3],
                       col_types = cols(
                         countyFIPS = col_double(),
                         `County Name` = col_character(),
                         State = col_character(),
                         population = col_double()
                       )) %>% 
  rename(county_name = `County Name`)

##### Data Cleaning #####

# Getting rid of unnecessary columns/rows and filtering to the 7 states we wants
cases <- cases[,-c(1,4)]
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

deaths <- deaths[,-c(1,4)]
deaths <- deaths %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

population <- population[,-1]
population <- population %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

# Formatting using the pivot_longer function
cases <- cases %>% 
  pivot_longer(!c(county_name, State), names_to = "date", values_to = "cases")

deaths <- deaths %>% 
  pivot_longer(!c(county_name, State), names_to = "date", values_to = "deaths")

# Converting cases and deaths to numeric. We imported as character because of 
# potential data entry errors that used a comma as a thousands-separator.
cases$cases <- str_remove_all(cases$cases, "[:punct:]")
deaths$deaths <- str_remove_all(deaths$deaths, "[:punct:]")
cases$cases <- as.numeric(cases$cases)
deaths$deaths <- as.numeric(deaths$deaths)

# Fixing Lac qui Parle County in Minnesota
cases$county_name <- str_replace_all(cases$county_name, "Lac Qui Parle", "Lac qui Parle")
population$county_name <- str_replace_all(population$county_name, "Lac Qui Parle", "Lac qui Parle")


# Joining the data
cases_and_deaths <- left_join(cases, deaths, 
                              by = c("county_name", "State", "date"))
cases_deaths_pop <- left_join(cases_and_deaths, population,
                              by = c("county_name", "State"))

# Making the case rate and death rate columns and renaming variables 
final_covid <- cases_deaths_pop %>% mutate(case_rate = cases/population*100000,
                                           death_rate = deaths/population*100000)

final_covid <- final_covid %>% rename(state = State)

# Fixing the date

final_covid$date <- as_date(final_covid$date, 
                            tz = "America/Chicago", 
                            format = "%m/%d/%y")


# creating new_cases, 7 day moving average, and 7 day average per 100K metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(new_cases = diff(c(0,cases)),
         moving_7_day_avg = roll_mean(new_cases, n = 7, fill = NA, align = "right"))
final_covid <- final_covid %>% 
  mutate(new_cases = if_else(new_cases < 0, 0, new_cases),
         avg_7_day_rate = moving_7_day_avg/population*100000)

## simplifying county lines
all_counties <- st_read("Data/All_counties.shp", type = 6)
states_map <- ms_simplify(all_counties, keep = 0.02)

## Getting states shape file data 
states_map2 <- st_read("Data/All_states.shp", type = 6)


#graphic_covid gives county_name as "Vanderburgh County" and a separate state column with "IN"
graphic_covid <- final_covid %>% 
  filter(!str_detect(county_name, "Statewide Unallocated"))

state_unallocated_data <- final_covid %>% 
  filter(str_detect(county_name, "Statewide Unallocated")) %>% 
  ungroup()

#state and their abbreviations
state_abb_to_name <- tibble(State = state.name, Abb = state.abb)

#Left joining covid and state names by their abbreviations
covid_data <- left_join(graphic_covid, state_abb_to_name, by = c("state"= "Abb"))

#Combine county_name and new state column with a comma between them to match format of states_map
covid_data <- covid_data %>% mutate(NAME = str_c(county_name, State, sep = ", "),
                                    name = str_c(county_name, state, sep = ", "))

covid_data_for_tweets<- covid_data %>% select(date, state, new_cases) %>%
  group_by(date, state) %>% summarize(state_new_cases=sum(new_cases))

manual_tweet_data<- covid_data_for_tweets %>% filter(date=="2020-11-18") 

dailycases<-str_c(manual_tweet_data$state_new_cases, sep=",")
states<- str_c(manual_tweet_data$state, sep=",")
tweet_initial<- str_c( states, dailycases, sep=":")
tweetwithspace<- str_c(tweet_initial, collapse="\n")
intro_to_tweet<- "Daily Cases by State (0 cases means the state does not report case data on this day of the week)"
tweetfinal<-str_c(intro_to_tweet, sep="\n\n", tweetwithspace)

post_tweet(tweetfinal)


