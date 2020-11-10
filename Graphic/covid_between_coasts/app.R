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
library(DT)
library(lubridate)
library(RColorBrewer)
library(RcppRoll) #for the roll_mean calculation of the 7-day moving average

##### Web Scraping #####

# Getting the csv files

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
  rename(County.Name = `County Name`)
deaths <- read_csv(covid_html_data[2],
                   col_types = cols(
                     .default = col_character(),
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

# Converting cases and deaths to numeric. We imported as character because of 
# potential data entry errors that used a comma as a thousands-separator.
cases$cases <- str_remove_all(cases$cases, "[:punct:]")
deaths$deaths <- str_remove_all(deaths$deaths, "[:punct:]")
cases$cases <- as.numeric(cases$cases)
deaths$deaths <- as.numeric(deaths$deaths)

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

# creating new_cases and 7 day moving average metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(new_cases = diff(c(0,cases)),
         moving_avg_7 = roll_mean(new_cases, n = 7, fill = NA, align = "right"))

##### Web Scraping #####

# Getting the csv files

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
  rename(County.Name = `County Name`)
deaths <- read_csv(covid_html_data[2],
                   col_types = cols(
                     .default = col_character(),
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

# Converting cases and deaths to numeric. We imported as character because of 
# potential data entry errors that used a comma as a thousands-separator.
cases$cases <- str_remove_all(cases$cases, "[:punct:]")
deaths$deaths <- str_remove_all(deaths$deaths, "[:punct:]")
cases$cases <- as.numeric(cases$cases)
deaths$deaths <- as.numeric(deaths$deaths)

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
states_map <- st_read("Data/All_counties.shp", type = 6)

#graphic_covid gives county_name as "Vanderburgh County" and a separate state column with "IN"
graphic_covid <- final_covid

#state and their abbrevations
state_abb_to_name <- tibble(State = state.name, Abb = state.abb)

#Left joining covid and state names by their abbrevations
covid_data <- left_join(graphic_covid, state_abb_to_name, by = c("state"= "Abb"))

#Combine county_name and new state column with a comma between them to match format of states_map
covid_data <- covid_data %>% mutate(NAME = str_c(county_name, State, sep = ', '))

#Joining two datasets
covid_map_data <- left_join(covid_data, states_map, by = "NAME")
covid_map_data <- st_as_sf(covid_map_data)

#Palette for leaflet
#In package RColorBrewer, RdYlGn goes from dark red to dark green
color_pal <- rev(brewer.pal(50, name="RdYlGn"))
pal_case <- colorNumeric(palette = color_pal, domain = covid_map_data$cases)

#Putting in new dataset for Statewide Unallocated

state_unallocated_data <- read_csv("Data/statewide_unallocated.csv")
state_unallocated_data$date <- mdy(state_unallocated_data$date)

#table for markers

City<- c("Chicago", "Indianapolis", "Detroit", "Louisville", "Milwaukee", "Columbus")
Lat<- c(41.8985, 39.7688, 42.3410, 38.2731, 43.0445, 39.9661)
Long<- c(-87.6341, -86.1649, -83.0630, -85.7627, -87.9109, -83.0029)
Link<- c("<a href='https://en.wikipedia.org/wiki/Chicago'> Chicago </a>",
         "<a href='https://en.wikipedia.org/wiki/Indianapolis'> Indianapolis </a>", 
         "<a href='https://en.wikipedia.org/wiki/Detroit'> Detroit </a>",
         "<a href='https://en.wikipedia.org/wiki/Louisville,_Kentucky'> Louisville </a>",
         "<a href='https://en.wikipedia.org/wiki/Milwaukee'> Milwaukee </a",
         "<a href='https://en.wikipedia.org/wiki/Columbus,_Ohio'> Columbus </a")

Marker <- data.frame(City, Lat, Long, Link)


legendvalues<- c(1:200000)


######################################################
# Define UI for application
# This is where you get to choose how the user sees
# the information and what they get to select and
# choose. 
######################################################
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID Between the Coasts"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "states", "Choose a State", c("All", "Kentucky", "Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin")),
  
      radioButtons(inputId = "stat", "Choose a Statistic", 
                   c("Total Cases" = "cases", 
                     "Total Deaths" = "deaths", 
                     "Case Rate per 100,000" = "case_rate",
                     "Death Rate per 100,000" = "death_rate")),
      
      sliderInput(inputId = "dates", "Timeline of COVID", 
              min = min(covid_map_data$date),
              max = max(covid_map_data$date),
              value = max(covid_map_data$date),
              timeFormat = "%m-%d-%Y",
              animate = TRUE),
  
      dateInput(inputId = "date_input", "Type in date you want to see", value = as.Date("06-24-2020","%m-%d-%Y"), format = "mm-dd-yyyy")

      
),

mainPanel(
  
  leafletOutput("map_cases"),
  
  helpText("A note on testing data: A case is defined as any individual

      
      
            who tests positive (via a PCR or antigen test) within a three month window.
            Serological tests do not count toward this total. For more on classifying cases,
           see", tags$a(href="https://wwwn.cdc.gov/nndss/conditions/coronavirus-disease-2019-covid-19/case-definition/2020/08/05/", 
                        "the CDC COVID Case Classification Page"),"."),

  h5(textOutput("title_unallocated")),

  tableOutput("unallocated")

  
))  


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

    dates <- reactive({
        covid_map_data %>% 
            filter(date == input$dates)
    })
    
    
    # code in here (inside the server function, but outside of a render function)
    # will run once per user. 
    
    # included Cases, Deaths, Case Rate, and Death Rate to leaflet


    output$map_cases <- renderLeaflet({
        if(input$stat %in% c("cases", "deaths", "case_rate", "death_rate")){
          stat <- switch(input$stat,
                         cases = dates()$cases,
                         deaths = dates()$deaths,
                         death_rate = dates()$death_rate,
                         case_rate = dates()$case_rate,
                         dates()$cases)
          data <- switch(input$stat,
                         cases = covid_map_data$cases,
                         deaths = covid_map_data$deaths,
                         death_rate = covid_map_data$death_rate,
                         case_rate = covid_map_data$case_rate,
                         covid_map_data$cases)
        pal_data <- colorNumeric(palette = "viridis", domain = data)
        leaflet(width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(data = st_transform(dates(), crs = "+init=epsg:4326"),
                    popup = str_c("<strong>", dates()$county_name, ", ", dates()$state,
                                  "</strong><br /> Cases: ", dates()$cases,
                                  "</strong><br /> Deaths: ", dates()$deaths,
                                  "</strong><br /> Case Rate: ", dates()$case_rate,
                                  "</strong><br /> Death Rate: ", dates()$death_rate),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.7,
                    color = ~ pal_data(stat)) %>%
        addMarkers(data = Marker,
                   ~Long, ~Lat, popup = ~as.character(Link), label = ~as.character(City)) %>%
        addLegend("bottomright",
                  pal = pal_data,
                  values = data,
                  title = str_to_title(str_replace(input$stat, "_", " ")),
                  opacity = 5)
    }
    })


  output$states <- renderText({input$states})
  
  output$stat <- renderText({input$stat})
  
  output$swun <- renderDataTable(sw)
  
  filtered_states_unallocated <- reactive({
      state_unallocated_data %>% 
      filter(date == input$dates) %>% 
      select(state, cases) %>% 
      rename(Cases = cases,
             State = state)
  })
  
  output$title_unallocated <- renderText({"Statewide Unallocated Cases:"})
  
  output$unallocated <- renderTable(
    pivot_wider(filtered_states_unallocated(), 
                names_from = "State",
                values_from = "Cases"),
    rownames = FALSE, 
    colnames = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
