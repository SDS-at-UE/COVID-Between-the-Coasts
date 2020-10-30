library(tidyverse)
library(shiny)

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

server <- function(input, output){
  output$states <- renderText({input$states})
  
  output$stat <- renderText({input$stat})
  
  
}

shinyApp(ui = ui, server = server)