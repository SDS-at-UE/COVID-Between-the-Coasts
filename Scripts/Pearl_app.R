library(shiny)
library(leaflet)

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
}

shinyApp(ui = ui, server = server)
