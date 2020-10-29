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


######################################################
# Define UI for application
# This is where you get to choose how the user sees
# the information and what they get to select and
# choose. 
######################################################
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
    
    # code in here (inside the server function, but outside of a render function)
    # will run once per user. 

  output$states <- renderText({input$states})
  
  output$stat <- renderText({input$stat})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
