# EDA App
## This app is designed to help explore the 
## demographic and COVID data for several states

library(shiny)
library(tidyverse)


IN_income <- read_csv("../Data/IN_income.csv",
                      col_types = cols(
                          County = col_character(),
                          State = col_character(),
                          variable = col_character(),
                          estimate = col_double(),
                          label = col_factor()
                      ))
IN_edu <- read_csv("../Data/IN_edu.csv",
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

map_data <- get_acs(geography = "county",
                    variables = "B25077_001",
                    state = "IN",
                    year = 2018,
                    geometry = TRUE)
map_data <- map_data %>% select(NAME, variable, estimate, geometry) %>% 
    separate(NAME, into = c("County", "State"), sep = " County,")
map_data <- left_join(map_data, variables_2018[, 1:2], by = "variable") %>% 
    filter(!variable %in% c("B19101_001"))
map_data$label <- as_factor(str_replace(map_data$label, ".*!!(.*)", "\\1"))


pal2 <- colorNumeric(palette = "viridis", domain = map_data$estimate)
popup <- paste0("<strong>", map_data$County,
                "</strong><br /> Median Home Value: ", map_data$estimate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: A Midwestern Reporting Project of WNIN and the University of Evansville."),

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
            addPolygons(popup = ~ popup,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal2(estimate)) %>% 
            addLegend("bottomright",
                      pal = pal2,
                      values = ~estimate,
                      title = "Median Home Value",
                      labFormat = labelFormat(prefix = "$"),
                      opacity = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
