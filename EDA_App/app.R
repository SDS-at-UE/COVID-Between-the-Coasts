# EDA App
## This app is designed to help explore the 
## demographic and COVID data for several states

library(shiny)
library(tidyverse)

IN_income <- read_csv("../Data/IN_income.csv")

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
           plotOutput("distPlot")
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
            select(County) %>% pull()
        selectInput("county",
                    "County",
                    county)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
