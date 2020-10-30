library(shiny)

pal_gini <- colorNumeric(palette = "viridis", domain = covid_map_data$Gini)

ui <- fluidPage(
  titlePanel("COVID-19: A Midwestern Reporting Project of WNIN and the University of Evansville."),
  h3("Exploratory Data Analysis"),
  selectInput(inputId = "state",
              label = "Choose a state",
              choices = c("Kentucky", "Illinois", 
                          "Indiana", "Michigan",
                          "Minnesota", "Ohio", 
                          "Wisconsin")),
  radioButtons(inputId = "covid_display_option",
               label = "COVID Rates",
               choices = c("Case Rate (per 100,000)", 
                           "Fatality Rate (per 100,000)",
                           "Cases",
                           "Deaths")),
  plotOutput(outputId = "state_plot", 
             width = 12,
             height = 12)
)

server <- function(input, output) {
  output$state <- renderText({input$state})
  output$covid_display_option <- renderText({input$covid_display_option})
  output$state_plot <- renderLeaflet({state_plot})
  
  output$map_gini <- renderLeaflet({
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
                  color = ~ pal_gini(Gini)) %>% 
      addLegend("bottomright",
                pal = pal_gini,
                values = ~ Gini,
                title = "COVID Between the Coasts",
                opacity = 1)
  })
  
  
}

shinyApp(ui = ui, server = server)
