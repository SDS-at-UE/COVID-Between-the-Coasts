#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

######################################################
# Everything in this section is run only once for the 
# whole application. Multiple users will take advantage
# of this code being run. 
# This is where you want to load up 
# your libraries, import your data, and perform any 
# calculations that will never change based on any user 
# input
######################################################

library(shiny)
library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(rvest)
library(lubridate)

##### Web Scraping #####

# Getting the csv files

covid_html_data <- read_html("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/") %>% 
  html_nodes('a') %>%
  html_attr('href') %>% 
  str_subset("\\.csv$")

# Extracting the data from the csv files 

cases <- read_csv(covid_html_data[1],
                  col_types = cols(
                    .default = col_double(),
                    `County Name` = col_character(),
                    State = col_character()
                  )) %>% 
  rename(County.Name = `County Name`)
deaths <- read_csv(covid_html_data[2],
                   col_types = cols(
                     .default = col_double(),
                     `County Name` = col_character(),
                     State = col_character()
                   )) %>% 
  rename(County.Name = `County Name`)
population <- read_csv(covid_html_data[3],
                       col_types = cols(
                         countyFIPS = col_double(),
                         `County Name` = col_character(),
                         State = col_character(),
                         population = col_double()
                       )) %>% 
  rename(County.Name = `County Name`)

##### Data Cleaning #####

# Getting rid of unnecessary columns/rows and filtering to the 7 states we wants
cases <- cases[,-c(1,4)]
cases <- cases %>% filter(!str_detect(`County.Name`, "Statewide Unallocated"))
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

deaths <- deaths[,-c(1,4)]
deaths <- deaths %>% filter(!str_detect(`County.Name`, "Statewide Unallocated"))
deaths <- deaths %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

population <- population[,-1]
population <- population %>% filter(!str_detect(`County.Name`, "Statewide Unallocated"))
population <- population %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

# Formatting using the pivot_longer function
cases <- cases %>% 
  pivot_longer(!c(County.Name, State), names_to = "date", values_to = "cases")

deaths <- deaths %>% 
  pivot_longer(!c(County.Name, State), names_to = "date", values_to = "deaths")

# Joining the data
cases_and_deaths <- merge(cases, deaths)
cases_deaths_pop <- merge(cases_and_deaths, population)

# Making the case rate and death rate columns and renaming variables 
final_covid <- cases_deaths_pop %>% mutate(case_rate = cases/population*100000,
                                           death_rate = deaths/population*100000)

final_covid <- final_covid %>% rename(county_name = County.Name,
                                      state = State)

# Fixing the date
final_covid$date <- as_date(final_covid$date, format = "%m/%d/%y")

## states_map gives NAME in format of "Vanderburgh County, Indiana"
states_map <- read_sf("Data/All_counties.shp", type = 6)

#graphic_covid gives county_name as "Vanderburgh County" and a separate state column with "IN"
graphic_covid <- final_covid

#state and their abbrevations
state_abb_to_name <- tibble(State = state.name, Abb = state.abb)

#Left joining covid and state names by their abbrevations
covid_data <- left_join(graphic_covid, state_abb_to_name, by = c("state"= "Abb"))

#Combine county_name and new state column with a comma between them to match format of states_map
covid_data <- covid_data %>% mutate(NAME = str_c(county_name, State, sep = ', '))

#Joining two datasets
covid_map_data <- geo_join(states_map, covid_data, by = "NAME")

#Palette for leaflet
#In package RColorBrewer, RdYlGn goes from dark red to dark green
pal_case <- colorNumeric(palette = "viridis", domain = covid_map_data$cases)

######################################################
# Define UI for application
# This is where you get to choose how the user sees
# the information and what they get to select and
# choose. 
######################################################
ui <- fluidPage(

  # Application title
  titlePanel("COVID Between the Coasts"),
  
  selectInput(inputId = "states", "Choose a State", c("All", "Kentucky", "Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin")),
  
  radioButtons(inputId = "stat", "Choose a Statistic", c("Total Cases", "Total Deaths", "Case Rate per 100,000", 
                                                         "Death Rate per 100,000", "Case Fatality Rate", "7 Day Moving Average")),
  
  sliderInput(inputId = "dates", "Timeline of COVID", 
              min = as.Date("01-01-2020","%m-%d-%Y"),
              max = as.Date("10-31-2020","%m-%d-%Y"),
              value=as.Date("06-24-2020","%m-%d-%Y")),
  
  dateInput(inputId = "date_input", "Type in date you want to see", value = as.Date("06-24-2020","%m-%d-%Y"), format = "mm-dd-yyyy") 
)


##################################################
# Define server logic
# This is where all of the R coding happens.
# This is where you take the user input and use it
# to make calculations, create graphics, to R stuff.
# This is where a majority of the code you've
# already seen/written would go if we were to 
# include it in the graphic. 
##################################################
server <- function(input, output) {
    
    # code in here (inside the server function, but outside of a render function)
    # will run once per user. 
    
    # included Cases, Deaths, Case Rate, and Death Rate to leaflet


    output$map_cases <- renderLeaflet({
        covid_map_data %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet(width = "100%") %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>% 
            addPolygons(popup = str_c("<strong>", covid_map_data$county_name, ", ", covid_map_data$state,
                                      "</strong><br /> Cases: ", covid_map_data$cases,
                                      "</strong><br /> Deaths: ", covid_map_data$deaths,
                                      "</strong><br /> Case Rate: ", covid_map_data$case_rate,
                                      "</strong><br /> Death Rate: ", covid_map_data$death_rate),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal_case(cases)) %>% 
            addLegend("bottomright",
                      pal = pal_case,
                      values = ~ cases,
                      title = "COVID Between the Coasts",
                      opacity = 1)
    })

  output$states <- renderText({input$states})
  
  output$stat <- renderText({input$stat})
  

}

# Run the application 
shinyApp(ui = ui, server = server)
