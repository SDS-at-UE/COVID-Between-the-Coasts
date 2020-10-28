library(tidyverse)
library(shiny)
library(leaflet)

ui <- fluidPage(
  # Application title
  titlePanel("COVID Between the Coasts"),
  selectInput(inputId = "states", "Choose a state", c("All", "Kentucky", "Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin")),
  radioButtons(inputId = "stat", "Choose a statistic", c("Total Cases", "Total Deaths", "Case Rate per 100,000", "Death Rate per 100,000")),
  plotOutput(outputId = "map")
)

server <- function(input, output){
  output$states <- renderText({input$states})
  
  output$stat <- renderText({input$stat})
  
  output$map <- renderLeaflet({})
  
}

shinyApp(ui = ui, server = server)