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


######################################################
# Define UI for application
# This is where you get to choose how the user sees
# the information and what they get to select and
# choose. 
######################################################
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
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
    
    # code in here (inside the server function, but outside of a render function)
    # will run once per user. 

    output$distPlot <- renderPlot({
    # code in here (inside a render function) will run many times per user.
    # You want to put as little code in here as possible, because it will run
    # over and over again for each individual user.
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
