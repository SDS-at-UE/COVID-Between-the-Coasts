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
library(rmapshaper)

#########################################
# The following chunk of code is designed
# to allow the map to respond quicker
# to changes in the date, i.e., a faster
# way to color the counties for each date.
###
# It comes from https://github.com/rstudio/leaflet/issues/496
# developed by @edwindj on https://github.com/rstudio/leaflet/pull/598
# and reworked by @timelyportfolio for use without the pull request.
# The label addition is an alteration of the one done by @martinzuba
#########################################

setShapeStyle <- function(map, data = getMapData(map), layerId,
                          stroke = NULL, color = NULL,
                          weight = NULL, opacity = NULL,
                          fill = NULL, fillColor = NULL,
                          fillOpacity = NULL, dashArray = NULL,
                          smoothFactor = NULL, noClip = NULL,
                          options = NULL){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip)))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

setShapeLabel <- function(map, data = getMapData(map), 
                          layerId,
                          label = NULL,
                          options = NULL){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}

### JS methods
leafletjs <-  tags$head(
  # add in methods from https://github.com/rstudio/leaflet/pull/598
  tags$script(HTML('
  window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  //label = HTMLWidgets.dataframeToD3(label);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      // layer.setStyle(style[i]);
      layer.unbindPopup();
      layer.bindPopup(label[i])
    }
  });
};
'
  ))
)

#########################################
# End of https://github.com/rstudio/leaflet/pull/598 code
#########################################

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


# creating new_cases and 7 day moving average metric
final_covid <- final_covid %>% 
  group_by(county_name, state) %>% 
  mutate(new_cases = diff(c(0,cases)),
         moving_avg_7 = roll_mean(new_cases, n = 7, fill = NA, align = "right"))
final_covid <- final_covid %>% mutate(new_cases = if_else(new_cases < 0, 0, new_cases))

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
covid_data <- covid_data %>% mutate(NAME = str_c(county_name, State, sep = ', '))

#Creating character vector for layerID in leaflet
layer_county <- unique(covid_data$NAME)

#Joining two datasets
covid_map_data <- left_join(covid_data, states_map, by = "NAME")
covid_map_data <- st_as_sf(covid_map_data)

#Palette for leaflet
#In package RColorBrewer, RdYlGn goes from dark red to dark green
color_pal <- rev(brewer.pal(11, name = "RdYlGn"))
#pal_case <- colorNumeric(palette = color_pal, domain = covid_map_data$cases)

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

table_caption <- as.character(shiny::tags$b("Statewide Unallocated Cases"))


legendvalues<- c(1:200000)


######################################################
# Define UI for application
# This is where you get to choose how the user sees
# the information and what they get to select and
# choose. 
######################################################
ui <- fluidPage(
  leafletjs, #incorporate https://github.com/rstudio/leaflet/pull/598 JavaScript
  
  # Application title
  titlePanel("COVID Between the Coasts"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "states", "Choose a State", c("All", "Kentucky", "Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin")),
      
      radioButtons(inputId = "stat", "Choose a Statistic", 
                   c("Total Cases" = "cases", 
                     "Total Deaths" = "deaths", 
                     "Case Rate per 100,000" = "case_rate",
                     "Death Rate per 100,000" = "death_rate",
                     "New Cases (Per Day)" = "new_cases",
                     "7 Day Average" = "moving_avg_7")),
      
      sliderInput(inputId = "dates", "Timeline of COVID", 
                  min = min(covid_map_data$date),
                  max = max(covid_map_data$date),
                  value = max(covid_map_data$date),
                  timeFormat = "%m-%d-%Y",
                  animate = animationOptions(interval = 350))#,
      
      #    dateInput(inputId = "date_input", "Type in date you want to see", value = as.Date("06-24-2020","%m-%d-%Y"), format = "mm-dd-yyyy"),
      
      
      
    ),
    
    mainPanel(
      
      leafletOutput("map_cases"),
      
      helpText("A note on testing data: A case is defined as any individual
            who tests positive (via a PCR or antigen test) within a three month window.
            Serological tests do not count toward this total. For more on classifying cases,
           see", tags$a(href="https://wwwn.cdc.gov/nndss/conditions/coronavirus-disease-2019-covid-19/case-definition/2020/08/05/", 
                        "the CDC COVID Case Classification Page"),"."),
      
      tableOutput("unallocated"),
      
      h5(helpText("Data Sources:")),
      
      helpText("COVID-19 data was obtained from",
               tags$a(href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", "USA Facts."),
               "County lines information was taken from the Census Bureau."),
      
      helpText("COVID Between the Coasts interactive map is powered by", tags$a(href="https://rstudio.com/", "RStudio."))
      
      
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
  
  reactive_data <-  reactive({
    switch(input$stat,
           cases = covid_map_data$cases,
           deaths = covid_map_data$deaths,
           death_rate = covid_map_data$death_rate,
           case_rate = covid_map_data$case_rate,
           new_cases = covid_map_data$new_cases,
           moving_avg_7 = covid_map_data$moving_avg_7,
           covid_map_data$cases)
  })
  
  reactive_stat <- reactive({
    switch(input$stat,
           cases = dates()$cases,
           deaths = dates()$deaths,
           death_rate = dates()$death_rate,
           case_rate = dates()$case_rate,
           new_cases = dates()$new_cases,
           moving_avg_7 = dates()$moving_avg_7,
           dates()$cases)
  })
  
  pal_data <- reactive({
    colorNumeric(palette = color_pal, domain = reactive_data())
  })
  
  popup_msg <- reactive({
    str_c("<strong>", dates()$county_name, ", ", dates()$state,
          "</strong><br /><strong>", format(dates()$date, "%m/%d/%Y"), "</strong>",
          "<br /> Cases: ", dates()$cases,
          "<br /> Deaths: ", dates()$deaths,
          "<br /> Case Rate: ", round(dates()$case_rate, 2),
          "<br /> Death Rate: ", round(dates()$death_rate, 2),
          "<br /> New Cases: ", dates()$new_cases,
          "<br /> 7 Day Average: ", round(dates()$moving_avg_7, 2))
  })
  
  
  output$map_cases <- renderLeaflet({
    leaflet(width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      addPolygons(data = st_transform(states_map2, crs = "+init=epsg:4326"),
                  group = "state",
                  color = "black",
                  fill = FALSE,
                  weight = 3) %>%
      addPolygons(data = st_transform(filter(covid_map_data, date == max(date)), crs = "+init=epsg:4326"),
                  layerId = layer_county,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7) %>%
      addMarkers(data = Marker,
                 ~Long, ~Lat, popup = ~as.character(Link), label = ~as.character(City))
  })
  
  observe({
    leafletProxy("map_cases", data = dates()) %>% 
      setShapeStyle(layerId = layer_county, 
                    fillColor = ~ pal_data()(reactive_stat()))
  })
  
  observe({
    leafletProxy("map_cases", data = dates()) %>% 
      setShapeLabel(layerId = layer_county,
                    label = popup_msg())
  })
  
  observe({
    leafletProxy("map_cases") %>% 
      clearControls() %>% 
      addLegend("bottomright",
                pal = pal_data(),
                values = reactive_data(),
                title = str_to_title(str_replace(input$stat, "_", " ")),
                opacity = 5)
  })
  
  
  filtered_states_unallocated <- reactive({
    state_unallocated_data %>% 
      filter(date == input$dates) %>% 
      select(state, cases) 
  })
  
  
  output$unallocated <- renderTable(
    pivot_wider(filtered_states_unallocated(), 
                names_from = "state",
                values_from = "cases"),
    rownames = FALSE,
    colnames = TRUE,
    digits = 0,
    caption = table_caption,
    caption.placement = "top")
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
