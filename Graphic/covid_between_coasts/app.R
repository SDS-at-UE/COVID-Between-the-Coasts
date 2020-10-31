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

## states_map gives NAME in format of "Vanderburgh County, Indiana"
states_map <- read_sf("Data/All_counties.shp", type = 6)

#graphic_covid gives county_name as "Vanderburgh County" and a separate state column with "IN"
graphic_covid <- read_csv("Data/graphic_covid.csv")

#state and their abbrevations
state_abb_to_name <- tibble(State = state.name, Abb = state.abb)

#Left joining covid and state names by their abbrevations
covid_data <- left_join(graphic_covid, state_abb_to_name, by = c("state"= "Abb"))

#Combine county_name and new state column with a comma between them to match format of states_map
covid_data <- covid_data %>% mutate(NAME = str_c(county_name, State, sep = ', '))

#Joining two datasets
#covid_map_data2 <- geo_join(states_map, covid_data, by = "NAME")
covid_map_data <- left_join(covid_data, states_map, by = "NAME")
covid_map_data <- st_as_sf(covid_map_data)

#Palette for leaflet
#In package RColorBrewer, RdYlGn goes from dark red to dark green
#pal_case <- colorNumeric(palette = "viridis", domain = covid_map_data$cases)

#table  for markers

City<- c("Chicago", "Indianapolis", "Detroit", "Louisville", "Milwaukee", "Columbus")
Lat<- c(41.8985, 39.7688, 42.3410, 38.2731, 43.0445, 39.9661)
Long<- c(-87.6341, -86.1649, -83.0630, -85.7627, -87.9109, -83.0029)
Link<- c("<a href='https://en.wikipedia.org/wiki/Chicago'> Chicago </a>", 
         "<a href='https://en.wikipedia.org/wiki/Indianapolis'> Indianapolis </a>", 
         "<a href='https://en.wikipedia.org/wiki/Detroit'> Detroit </a>",
         "<a href='https://en.wikipedia.org/wiki/Louisville,_Kentucky'> Louisville </a>",
         "<a href='https://en.wikipedia.org/wiki/Milwaukee'> Milwaukee </a",
         "<a href='https://en.wikipedia.org/wiki/Columbus,_Ohio'> Columbus </a")

Marker<-data.frame(City, Lat, Long, Link)

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
  
      radioButtons(inputId = "stat", "Choose a Statistic", c("Total Cases", "Total Deaths", "Case Rate per 100,000", 
                                                         "Death Rate per 100,000", "Case Fatality Rate", "7 Day Moving Average")),
  
      sliderInput(inputId = "dates", "Timeline of COVID", 
              min = as.Date("01-01-2020","%m-%d-%Y"),
              max = as.Date("10-31-2020","%m-%d-%Y"),
              value=as.Date("06-24-2020","%m-%d-%Y"),
              animate = TRUE),
  
      dateInput(inputId = "date_input", "Type in date you want to see", value = as.Date("06-24-2020","%m-%d-%Y"), format = "mm-dd-yyyy") 
),
xyz <- mainPanel(
  leafletOutput("mymap"),

  fluidPage(
    leafletOutput("map_cases")
   
    
  ))  

))


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
            filter(date == as.character("8/22/20"))
    })
    
    pal_case <- reactive({
      colorNumeric(palette = "viridis", domain = dates()$cases)
    })
    
    
    # code in here (inside the server function, but outside of a render function)
    # will run once per user. 
    
    # included Cases, Deaths, Case Rate, and Death Rate to leaflet


    output$map_cases <- renderLeaflet({
        dates() %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet(width = "100%") %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>% 
            addPolygons(popup = str_c("<strong>", dates()$county_name, ", ", dates()$state,
                                      "</strong><br /> Cases: ", dates()$cases,
                                      "</strong><br /> Deaths: ", dates()$deaths,
                                      "</strong><br /> Case Rate: ", dates()$case_rate,
                                      "</strong><br /> Death Rate: ", dates()$death_rate),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal_case()(cases)) %>% 
            addLegend("bottomright",
                      pal = pal_case(),
                      values = ~ cases,
                      title = "COVID Between the Coasts",
                      opacity = 1)
    })

  output$states <- renderText({input$states})
  
  output$stat <- renderText({input$stat})
  
  
  output$mymap<- renderLeaflet({
  markers<- leaflet(data = Marker) %>% addTiles() %>%
    addMarkers(~Long, ~Lat, popup = ~as.character(Link), label = ~as.character(City))})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
