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
library(plotly)


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

locate_cases <- str_which(covid_html_data, "confirmed")[1]
locate_deaths <- str_which(covid_html_data, "deaths")[1]
locate_population <- str_which(covid_html_data, "population")[1]

cases <- read_csv(covid_html_data[locate_cases],
                  col_types = cols(
                    .default = col_character(),
                    `County Name` = col_character(),
                    State = col_character()
                  )) %>% 
  rename(county_name = `County Name`)
deaths <- read_csv(covid_html_data[locate_deaths],
                   col_types = cols(
                     .default = col_character(),
                     `County Name` = col_character(),
                     State = col_character()
                   )) %>% 
  rename(county_name = `County Name`)
population <- read_csv(covid_html_data[locate_population],
                       col_types = cols(
                         countyFIPS = col_double(),
                         `County Name` = col_character(),
                         State = col_character(),
                         population = col_double()
                       )) %>% 
  rename(county_name = `County Name`)

##### Data Cleaning #####

# Getting rid of unnecessary columns/rows and filtering to the 7 states we wants
cases <- cases %>% select(-ends_with("FIPS"))
cases <- cases %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

deaths <- deaths %>% select(-ends_with("FIPS"))
deaths <- deaths %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

population <- population %>% select(-ends_with("FIPS"))
population <- population %>% filter(State %in% c("IN", "KY", "MI", "OH", "IL", "WI", "MN"))

# Formatting using the pivot_longer function
cases <- cases %>% 
  pivot_longer(!c(county_name, State), names_to = "date", values_to = "cases", names_transform = list(date = as_date))

deaths <- deaths %>% 
  pivot_longer(!c(county_name, State), names_to = "date", values_to = "deaths", names_transform = list(date = as_date))

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

# final_covid$date <- as_date(final_covid$date, 
#                             tz = "America/Chicago", 
#                             format = "%m/%d/%y")


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

#Creating character vector for layerID in leaflet
layer_county <- unique(covid_data$NAME)

#Joining two datasets
covid_map_data <- left_join(covid_data, states_map, by = "NAME", copy = TRUE) 
covid_map_data <- st_as_sf(covid_map_data)

#Palette for leaflet
## Make vector of colors for first bin
color_pal1 <- colorRampPalette(colors = c("springgreen4", "yellow3"), space = "Lab")(3)

## Make vector of colors for second bin
color_pal2 <- colorRampPalette(colors = c("yellow3", "orange"), space = "Lab")(9)

## Make vector of colors for third bin
color_pal3 <- colorRampPalette(colors = c("orange", "red3"), space = "Lab")(8)

## Make vector of colors for fourth bin
color_pal4 <- colorRampPalette(colors = c("red3", "darkred"), space = "Lab")(175)

## Make vector of colors for last bin
color_pal5 <- colorRampPalette(colors = c("darkred", "black"), space = "Lab")(5)

## Combine the five color palettes
color_pal <- c(color_pal1, color_pal2, color_pal3, color_pal4, color_pal5)

#table for markers

City <- c("Champaign", "Minneapolis", "Chicago", "Indianapolis", "Detroit", "Louisville", "Milwaukee", "Columbus")
Lat <- c(40.1164, 44.9778, 41.8985, 39.7688, 42.3410, 38.2731, 43.0445, 39.9661)
Long <- c(-88.2434, -93.2650, -87.6341, -86.1649, -83.0630, -85.7627, -87.9109, -83.0029)
Link <- c("<a href='https://news.wnin.org/post/cbc-s1-e8-covid-casts-long-shadow-over-sports-scholarships'> Ep. 8 COVID Interrupts Athletes' Dreams </a>",
          "<a href='https://news.wnin.org/post/cbc-s1-e4-minneapolis-supply-chain'> Ep. 4 The Supply Chain </a>",
          "<a href='https://news.wnin.org/post/cbc-s1-e3-chicago-tribute-essential-workers'> Ep. 3 A Tribute to Essential Workers </a>",
          "<a href='https://news.wnin.org/post/cbc-s1-e6-covid-countryside#stream/0'> Ep. 6 COVID in the Countryside </a>", 
          "<a href='https://news.wnin.org/post/cbc-s1-e2-detroit-day-district-five'> Ep. 2 A Day in District 5 </a>",
          "<a href='https://news.wnin.org/post/cbc-s1-e7-borders#stream/0'> Ep. 7 Borders </a>",
          "<a href='https://news.wnin.org/post/cbc-s1-e5-covid-numbers'> Ep. 5 COVID By the Numbers </a",
          "<a href='https://news.wnin.org/post/cbc-s1-e1-survivor-stories'> Ep. 1 Survivor Stories </a")

Marker <- data.frame(City, Lat, Long, Link)

# Make the icons based on episode order from Marker object
marker_icons <- icons(
  iconUrl = if_else(Marker$City == "Champaign",
                    "www/ep8.jpg",
                    if_else(Marker$City == "Minneapolis",
                            "www/ep4.jpg",
                            if_else(Marker$City == "Chicago",
                                    "www/ep3.jpg",
                                    if_else(Marker$City == "Indianapolis",
                                            "www/ep6.jpg",
                                            if_else(Marker$City == "Detroit",
                                                    "www/ep2.jpg",
                                                    if_else(Marker$City == "Louisville",
                                                            "www/ep7_new.jpg",
                                                            if_else(Marker$City == "Milwaukee",
                                                                    "www/ep5.png",
                                                                    "www/ep1.jpg"))))
                            )
                    )),
  iconWidth = 45, iconHeight = 45,
  iconAnchorX = 1, iconAnchorY = 45,
)

table_caption <- as.character(shiny::tags$b("Statewide Unallocated Cases"))


######################################################
# Define UI for application
# This is where you get to choose how the user sees
# the information and what they get to select and
# choose. 
######################################################
ui <- fluidPage(
  leafletjs, #incorporate https://github.com/rstudio/leaflet/pull/598 JavaScript
  
  tabsetPanel(
    tabPanel("COVID Timeline",
             wellPanel(
               fluidRow(
                 column(width = 3, align = "center", tags$img(src = "CovidBetweentheCoastsLogo_crop.png", height = "90")),
                 column(width = 6,
                        sliderInput(inputId = "dates", "Timeline of COVID", 
                                    min = min(covid_map_data$date),
                                    max = max(covid_map_data$date),
                                    value = max(covid_map_data$date),
                                    timeFormat = "%m-%d-%Y",
                                    step = 4,
                                    animate = animationOptions(interval = 300))
                 ),column(width = 3, 
                          selectInput(inputId = "stat", "Choose a Statistic", 
                                      c("Total Cases" = "cases", 
                                        "Total Deaths" = "deaths", 
                                        "Case Rate per 100,000" = "case_rate",
                                        "Death Rate per 100,000" = "death_rate",
                                        "New Cases (Per Day)" = "new_cases",
                                        "7 Day Average" = "moving_7_day_avg",
                                        "7 Day Avg Rate" = "avg_7_day_rate"),
                                      selected = "avg_7_day_rate"),
                          checkboxInput(inputId = "marker", "Show stories?",
                                        TRUE))
               ),
               fluidRow(
                 h5("Choose a COVID-19 statistic from the dropdown menu and see how it spread across our region.
          Click on any county to see COVID-19 information for the date selected. Click on an image to 
          take you to one of our episodes.")
               )),
             
             leafletOutput("map_cases", height = 525),
             
             helpText(HTML('A note on testing data: A case is defined as any individual
                who tests positive (via a PCR or antigen test) within a three month window.
                Serological tests do not count toward this total. For more on classifying cases,
                see the 
                <a href="https://wwwn.cdc.gov/nndss/conditions/coronavirus-disease-2019-covid-19/case-definition/2020/08/05/">
                CDC COVID Case Classification Page</a>. Some cases were not attributed to a county. 
                These are given in the table below.')),
             
             tableOutput("unallocated")   
    ),
    tabPanel("COVID by County",
             wellPanel(
               fluidRow(
                 column(width = 3, align = "center", tags$img(src = "CovidBetweentheCoastsLogo_crop.png", height = "90")),
                 column(width = 3,
                        selectInput("state1",
                                    "Select State 1",
                                    choices = c("Choose one" = "", unique(covid_data$State))),
                        uiOutput("county1"),
                 ),
                 column(width = 3,
                        selectInput("state2",
                                    "Select State 2",
                                    choices = c("Optional" = "", unique(covid_data$State)),
                                    selectize = FALSE),
                        uiOutput("county2"),
                 ),
                 column(width = 3, 
                        selectInput(inputId = "stat2", "Choose a Statistic", 
                                    c("Total Cases" = "cases", 
                                      "Total Deaths" = "deaths", 
                                      "Case Rate per 100,000" = "case_rate",
                                      "Death Rate per 100,000" = "death_rate",
                                      "New Cases (Per Day)" = "new_cases",
                                      "7 Day Average" = "moving_7_day_avg",
                                      "7 Day Avg Rate" = "avg_7_day_rate"),
                                    selected = "avg_7_day_rate")
                 )
               ),
               fluidRow(
                 h5("Select a state, a county, and a statistic to see its progression since the beginning of
                    the pandemic. Want to compare two counties? Use the State 2 and County 2 dropdown menus.")
               )
             ),
             conditionalPanel(
               condition = "input.county1",
               plotlyOutput("plot", height = "500px")
             )
             
    ),
    
    div(align = "center",
        class = "footer",
        wellPanel(
          helpText(HTML('COVID-19 data was obtained from 
                      <a href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/">USA Facts</a>.
                      County boundaries were taken from the Census Bureau and simplified for better rendering. 
                      COVID Between the Coasts interactive app is powered by
                      <a href="https://www.rstudio.com/products/connect/">RStudio Connect</a> and was developed by Maya Frederick, 
                      Timmy Miller, Ethan Morlock, and Pearl Muensterman, students at the
                      <a href="https://www.evansville.edu/">University of Evansville</a> 
                      led by Dr. Darrin Weber.'))
        )
    ) 
  )
  
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
           moving_7_day_avg = covid_map_data$moving_7_day_avg,
           avg_7_day_rate = covid_map_data$avg_7_day_rate,
           covid_map_data$cases)
  })
  
  reactive_stat <- reactive({
    switch(input$stat,
           cases = dates()$cases,
           deaths = dates()$deaths,
           death_rate = dates()$death_rate,
           case_rate = dates()$case_rate,
           new_cases = dates()$new_cases,
           moving_7_day_avg = dates()$moving_7_day_avg,
           avg_7_day_rate = dates()$avg_7_day_rate,
           dates()$cases)
  })
  
  output$county1 <- renderUI({
    county1 <- filter(covid_data, State == input$state1) %>% 
      ungroup() %>% 
      select(county_name) %>% 
      distinct() %>%
      arrange(county_name) %>%  
      pull()
    selectInput("county1",
                "Select County 1",
                choices = county1,
                selected = NULL)
  })
  
  output$county2 <- renderUI({
    county2 <- filter(covid_data, State == input$state2) %>% 
      ungroup() %>% 
      select(county_name) %>% 
      distinct() %>%
      arrange(county_name) %>%  
      pull()
    selectInput("county2",
                "Select County 2",
                choices = county2,
                selected = NULL)
  })
  
  counties <- reactive({
    covid_data %>% 
      filter(NAME %in% c(str_c(input$county1, input$state1, sep = ", "),
                         str_c(input$county2, input$state2, sep = ", ")))
  })
  
  reactive_title_for_county1 <- reactive({
    switch(input$state1,
           Illinois = "IL",
           Indiana = "IN",
           Kentucky = "KY",
           Michigan = "MI",
           Minnesota = "MN",
           Ohio = "OH",
           Wisconsin = "WI")
  })
  
  reactive_title_for_county2 <- reactive({
    switch(input$state2,
           Illinois = "IL",
           Indiana = "IN",
           Kentucky = "KY",
           Michigan = "MI",
           Minnesota = "MN",
           Ohio = "OH",
           Wisconsin = "WI")
  })
  
  reactive_data2_titles <-  reactive({
    switch(input$stat2,
           cases = "Total Number of Cases",
           deaths = "Total Number of Deaths",
           death_rate = "Number of Deaths per 100,000",
           case_rate = "Number of Cases per 100,000",
           new_cases = "Number of New Cases (by day)",
           moving_7_day_avg = "Seven Day Average of New Cases",
           avg_7_day_rate = "Seven Day Average of New Cases per 100,000",
           "Total Number of Cases")
  })
  
  reactive_data2_y_axis_label <-  reactive({
    switch(input$stat2,
           cases = "Cases",
           deaths = "Deaths",
           death_rate = "Deaths per 100,000",
           case_rate = "Cases per 100,000",
           new_cases = "Cases",
           moving_7_day_avg = "Cases",
           avg_7_day_rate = "Cases per 100,000",
           "Cases")
  })
  
  plot_county_title <- reactive({
    if(input$county2 == ""){
      str_c("COVID in ", str_c(input$county1, reactive_title_for_county1(), sep = ", "))
    } else{
      str_c("COVID in ", str_c(input$county1, reactive_title_for_county1(), sep = ", "),
            " and ", str_c(input$county2, reactive_title_for_county2(), sep = ", "))
    }
  })
  
  pal_data <- reactive({
    colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
    # colorNumeric(palette = color_pal, domain = reactive_data())
  })
  
  popup_msg <- reactive({
    str_c("<strong>", dates()$county_name, ", ", dates()$state,
          "</strong><br /><strong>", format(dates()$date, "%m/%d/%Y"), "</strong>",
          "<br /> Cases: ", dates()$cases,
          "<br /> Deaths: ", dates()$deaths,
          "<br /> Case Rate: ", round(dates()$case_rate, 2),
          "<br /> Death Rate: ", round(dates()$death_rate, 2),
          "<br /> New Cases: ", dates()$new_cases,
          "<br /> 7 Day Average: ", round(dates()$moving_7_day_avg, 2),
          "<br /> 7 Day Avg Rate: ", round(dates()$avg_7_day_rate, 2))
  })
  
  
  output$map_cases <- renderLeaflet({
    leaflet(width = "100%",
            options = leafletOptions(zoomSnap = 0,
                                     zoomDelta = 0.25)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      setView(lat = 43.0445, lng = -87.9109, zoom = 5.5) %>%
      addPolygons(data = st_transform(states_map2, crs = "+init=epsg:4326"),
                  group = "state",
                  color = "black",
                  fill = FALSE,
                  weight = 3) %>%
      addPolygons(data = st_transform(filter(covid_map_data, date == max(date)), crs = "+init=epsg:4326"),
                  layerId = layer_county,
                  color = "white",
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7)
  })
  
  observe({
    if(input$marker == TRUE){
      leafletProxy("map_cases")  %>%
        addMarkers(data = Marker,
                   ~Long, ~Lat, 
                   popup = ~as.character(Link), 
                   label = ~as.character(City),
                   icon = marker_icons)
    } else{
      leafletProxy("map_cases") %>% 
        clearMarkers()
    }
  })
  
  observe({
    leafletProxy("map_cases", data = dates()) %>% 
      setShapeStyle(layerId = layer_county, 
                    fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
  })
  
  observe({
    leafletProxy("map_cases", data = dates()) %>% 
      setShapeLabel(layerId = layer_county,
                    label = popup_msg())
  })
  
  observe({
    leafletProxy("map_cases") %>% 
      clearControls() %>% 
      addLegend("bottomleft",
                pal = pal_data(),
                values = na.omit(reactive_data()),
                title = str_to_title(str_replace_all(input$stat, "_", " ")),
                na.label = "",
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
  
  output$plot <- renderPlotly({
    ggplotly(
      ggplot(counties(), 
             aes_string(x = "date", 
                        color = "name", 
                        y = input$stat2)) +
        geom_point() +
        geom_smooth(se = FALSE, 
                    method = "gam", 
                    formula = y ~ s(x, bs = "cs", k = 30)) +
        scale_x_date(date_labels = "%m/%d/%y", date_breaks = "2 weeks") +
        scale_y_continuous(n.breaks = 8) +
        labs(x = "Date", 
             y = reactive_data2_y_axis_label(),
             title = plot_county_title(),
             subtitle = reactive_data2_titles(),
             color = "Selected Counties") +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45,
                                         hjust = 1),
              axis.text = element_text(size = 8),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 18),
              plot.subtitle = element_text(size = 16),
              legend.text = element_text(size = 10))
    ) %>%
      layout(legend = list(orientation = "h",  
                           xanchor = "center",
                           x = 0.5,
                           y = -0.25),
             title = list(text = paste0(plot_county_title(),
                                        "<br>", "<sup>", reactive_data2_titles(), "</sup>"))) %>% 
      config(modeBarButtonsToRemove = list("hoverCompareCartesian"))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
