# EDA App
## This app is designed to help explore the 
## demographic and COVID data for several states

library(shiny)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(stringr)
library(sf)
library(tigris)

census_api_key("7cf0c318e343f70900ce428bc2646b7f776807e5")

variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>% 
    rename(variable = name)

IN_income <- read_csv("Data/IN_income.csv",
                      col_types = cols(
                          County = col_character(),
                          State = col_character(),
                          variable = col_character(),
                          estimate = col_double(),
                          label = col_factor()
                      ))
IN_edu <- read_csv("Data/IN_edu.csv",
                   col_types = cols(
                       County = col_character(),
                       State = col_character(),
                       variable = col_character(),
                       estimate = col_double(),
                       Sex = col_factor(),
                       Age = col_factor(),
                       Education = col_factor()
                   ))

### Map Data/Code

covid_cases <- read_csv("Data/covid_cases.csv")
states_map <- read_sf("Data/All_counties.shp", type = 6)
map_data <- geo_join(states_map, covid_cases, by = "NAME") %>%
    separate(NAME, into = c("County", "State"), sep = " County, ")

map_data_sans_Cook <- filter(map_data, County != "Cook" | State != "Illinois")
map_data_Cook <- filter(map_data, County == "Cook", State == "Illinois")

# map_data <- get_acs(geography = "county",
#                     variables = "B25077_001",
#                     state = "IN",
#                     year = 2018,
#                     geometry = TRUE)
# map_data <- map_data %>% select(NAME, variable, estimate, geometry) %>% 
#     separate(NAME, into = c("County", "State"), sep = " County,")
# map_data <- left_join(map_data, variables_2018[, 1:2], by = "variable") %>% 
#     filter(!variable %in% c("B19101_001"))
# map_data$label <- as_factor(str_replace(map_data$label, ".*!!(.*)", "\\1"))


pal <- colorNumeric(palette = "viridis", domain = map_data_sans_Cook$Cases)
pal_Cook <- colorNumeric("Black", domain = map_data_Cook$Cases)
popup_msg <- paste0("<strong>", map_data_sans_Cook$County, " County, ", map_data_sans_Cook$State,
                "</strong><br /> Confirmed Cases: ", map_data_sans_Cook$Cases)
popup_msg_Cook <- paste0("<strong>", map_data_Cook$County, " County, ", map_data_Cook$State,
                    "</strong><br /> Confirmed Cases: ", map_data_Cook$Cases)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: A Midwestern Reporting Project of WNIN and the University of Evansville."),
    h3("Exploratory Data Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state",
                        "State",
                        IN_income$State),
            uiOutput("county")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map"),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("income_plot"),
                            plotOutput("edu_plot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$county <- renderUI({
        county <- filter(IN_income, State == input$state) %>% 
            select(County) %>% 
            arrange(County) %>% 
            pull()
        selectInput("county",
                    "County",
                    county)
    })
    
    output$income_plot <- renderPlot({
        ggplot(filter(IN_income, 
                      State == input$state,
                      County == input$county)) + 
            geom_col(aes(x = label, y = estimate)) +
            labs(x = "Family Income", 
                 y = "Number of People",
                 title = str_c("Distribution of Income in ", input$county, " County, ", input$state)) +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1))
    })
    
    output$edu_plot <- renderPlot({
        ggplot(filter(IN_edu, 
                      State == input$state,
                      County == input$county)) + 
            geom_col(aes(x = Education, y = estimate)) +
            labs(x = "Level of Education", 
                 y = "Number of People",
                 title = str_c("Educational Attainment in ", input$county, " County, ", input$state)) +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1))
    })
    
    output$map <- renderLeaflet({
        map_data %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet(width = "100%") %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>% 
            addPolygons(data = st_transform(map_data_sans_Cook, crs = "+init=epsg:4326"),
                        popup = ~ popup_msg,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(Cases)) %>%
            addPolygons(data = st_transform(map_data_Cook, crs = "+init=epsg:4326"),
                        popup = ~ popup_msg_Cook,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = "03F") %>% 
            addLegend("bottomright",
                      pal = pal,
                      values = ~ Cases,
                      title = "Confirmed Cases",
                      opacity = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
