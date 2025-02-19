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

# Demographic Data

income <- read_csv("Data/income.csv",
                   col_types = cols(
                       NAME = col_character(),
                       County = col_character(),
                       State = col_character(),
                       variable = col_character(),
                       estimate = col_double(),
                       label = col_factor()
                   ))

edu <- read_csv("Data/edu.csv",
                col_types = cols(
                    NAME = col_character(),
                    County = col_character(),
                    State = col_character(),
                    variable = col_character(),
                    estimate = col_double(),
                    Sex = col_factor(),
                    Age = col_factor(),
                    Education = col_factor()
                ))

ethnic <- read_csv("Data/ethnic.csv",
                   col_types = cols(
                       NAME = col_character(),
                       County = col_character(),
                       State = col_character(),
                       variable = col_character(),
                       estimate = col_double(),
                       Hispanic_Latino = col_factor(),
                       Race = col_factor()
                   ))

employ <- read_csv("Data/employ.csv",
                   col_types = cols(
                       NAME = col_character(),
                       County = col_character(),
                       State = col_character(),
                       variable = col_character(),
                       estimate = col_double(),
                       Sex = col_factor(),
                       Age = col_factor(),
                       Employment = col_factor()
                   ))

race <- read_csv("Data/race.csv",
                 col_types = cols(
                     NAME = col_character(),
                     County = col_character(),
                     State = col_character(),
                     variable = col_character(),
                     estimate = col_double(),
                     label = col_character()
                 ))

sex_age <- read_csv("Data/sex_age.csv",
                    col_types = cols(
                        NAME = col_character(),
                        County = col_character(),
                        State = col_character(),
                        variable = col_character(),
                        estimate = col_double(),
                        Sex = col_factor(),
                        Age = col_factor(),
                        HI_Coverage = col_factor()
                    ))

health_public <- read_csv("Data/health_public.csv",
                          col_types = cols(
                              NAME = col_character(),
                              County = col_character(),
                              State = col_character(),
                              variable = col_character(),
                              estimate = col_double(),
                              Sex = col_factor(),
                              Age = col_factor(),
                              Public_HI = col_factor()
                          ))

health_private <- read_csv("Data/health_private.csv",
                           col_types = cols(
                               NAME = col_character(),
                               County = col_character(),
                               State = col_character(),
                               variable = col_character(),
                               estimate = col_double(),
                               Sex = col_factor(),
                               Age = col_factor(),
                               Public_HI = col_factor()
                          ))

### Map Data/Code

states_map <- read_sf("Data/All_counties.shp", type = 6)

covid_data <- read_csv("Data/covid_data.csv",
                       col_types = cols(
                           NAME = col_character(),
                           County = col_character(),
                           State = col_character(),
                           State_abb = col_character(),
                           Cases = col_double(),
                           Deaths = col_double(),
                           Population = col_double(),
                           case_fatality = col_double(),
                           death_rate = col_double(),
                           case_rate = col_double()
                       ))

gini <- read_csv("Data/gini.csv",
                 col_types = cols(
                     NAME = col_character(),
                     County = col_character(),
                     State = col_character(),
                     variable = col_character(),
                     estimate = col_double(),
                     label = col_character()
                 ))

outlier_counties <- c("Cook County, Illinois",
                      "Wayne County, Michigan")
covid_data <- left_join(covid_data, select(gini, NAME, Gini = estimate), by = "NAME")
covid_map_data <- geo_join(states_map, covid_data, by = "NAME")
covid_map_data_sans_Cook <- filter(covid_map_data, !(NAME %in% outlier_counties))
covid_map_data_Cook <- filter(covid_map_data, NAME %in% outlier_counties)

pal <- colorNumeric(palette = "viridis", domain = covid_map_data_sans_Cook$Cases)
pal_death_sans_Cook <- colorNumeric(palette = "viridis", domain = covid_map_data_sans_Cook$Deaths)
pal_gini <- colorNumeric(palette = "viridis", domain = covid_map_data$Gini)


popup_msg <- str_c("<strong>", covid_map_data_sans_Cook$County, ", ", covid_map_data_sans_Cook$State_abb,
                   "</strong><br /> Cases: ", covid_map_data_sans_Cook$Cases,
                   "<br /> Deaths: ", covid_map_data_sans_Cook$Deaths,
                   "<br /> Case Fatality: ", covid_map_data_sans_Cook$case_fatality, " %",
                   "<br /> Cases per capita: ", covid_map_data_sans_Cook$case_rate,
                   "<br /> Deaths per capita: ", covid_map_data_sans_Cook$death_rate)
popup_msg_Cook <- str_c("<strong>", covid_map_data_Cook$County, ", ", covid_map_data_Cook$State_abb,
                        "</strong><br /> Cases: ", covid_map_data_Cook$Cases,
                        "<br /> Deaths: ", covid_map_data_Cook$Deaths,
                        "<br /> Case Fatality: ", covid_map_data_Cook$case_fatality, " %",
                        "<br /> Cases per capita: ", covid_map_data_Cook$case_rate,
                        "<br /> Deaths per capita: ", covid_map_data_Cook$death_rate)


ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: A Midwestern Reporting Project of WNIN and the University of Evansville."),
    h3("Exploratory Data Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("rate",
                        "Which graph?",
                        c("Case Fatality Rate (as %)" = "case_fatality", 
                          "Death rate (per 100,000)" = "death_rate", 
                          "Case rate (per 100,000)" = "case_rate",
                          "Confirmed Cases" = "Cases",
                          "Deaths" = "Deaths")),
            selectInput("state",
                        "State",
                        income$State),
            uiOutput("county"),
            radioButtons("ethnic_race",
                         "Ethnic/Race Display",
                         c("Race by Ethnicity" = "facet_ethnic",
                           "Ethnicity by Race" = "facet_race"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            h4("COVID Rates by County (select to the left)"),
                            h4("Gini Index by County"))
            ),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            leafletOutput("map_rates"),
                            leafletOutput("map_gini"))
            ),
            tags$hr(),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("income_plot"),
                            plotOutput("edu_plot"))
            ),
            tags$hr(),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("ethnic_plot"),
                            plotOutput("employ_plot"))
            ),
            tags$hr(),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("race_plot"),
                            plotOutput("sex_age_plot"))
            ),
            tags$hr(),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("health_public_plot"),
                            plotOutput("health_private_plot"))
            )
        )
    )
)


server <- function(input, output) {
    
    output$county <- renderUI({
        county <- filter(income, State == input$state) %>% 
            select(County) %>% 
            arrange(County) %>% 
            pull()
        selectInput("county",
                    "County",
                    county)
    })
    
    output$income_plot <- renderPlot({
        ggplot(filter(income,
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
        ggplot(filter(edu, 
                      State == input$state,
                      County == input$county)) + 
            geom_col(aes(x = Education, y = estimate)) +
            labs(x = "Level of Education", 
                 y = "Number of People",
                 title = str_c("Educational Attainment in ", input$county, " County, ", input$state)) +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1))
        })
    
    output$ethnic_plot <- renderPlot({
        if(input$ethnic_race == "facet_ethnic"){
            ggplot(filter(ethnic,
                          State == input$state,
                          County == input$county)) +
                geom_col(aes(x = Race, y = estimate)) +
                facet_grid(~ Hispanic_Latino,
                           labeller = labeller(Hispanic_Latino = c(Yes = "Hispanic or Latino",
                                                                   No = "Not Hispanic or Latino"))) +
                labs(x = "Race by Ethnicity",
                     y = "Number of People",
                     title = str_c("Ethnicity/Race in ", input$county, " County, ", input$state)) +
                theme(axis.text.x = element_text(angle = 45,
                                                 hjust = 1))
        } else{
            ggplot(filter(ethnic,
                          State == input$state,
                          County == input$county)) +
                geom_col(aes(x = Hispanic_Latino, y = estimate)) +
                facet_wrap(~ Race) +
                labs(x = "Ethnicity by Race",
                     y = "Number of People",
                     title = str_c("Ethnicity/Race in ", input$county, " County, ", input$state)) +
                theme(axis.text.x = element_text(angle = 45,
                                                 hjust = 1)) +
                scale_x_discrete(labels = c("Yes" = "Hispanic/Latino",
                                            "No" = "Not"))
        }
        
    })
    
    output$employ_plot <- renderPlot({
        ggplot(filter(employ,
                      State == input$state,
                      County == input$county)) +
            geom_col(aes(x = Age, y = estimate)) +
            facet_wrap(~ Employment) +
            labs(x = "Age by Employment Status",
                 y = "Number of People",
                 title = str_c("Employment Status in ", input$county, " County, ", input$state)) +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1))
    })
    
    output$race_plot <- renderPlot({
        ggplot(filter(race,
                      State == input$state,
                      County == input$county)) +
            geom_col(aes(x = label, y = estimate)) +
            labs(x = "Race",
                 y = "Number of People",
                 title = str_c("Race Breakdown in ", input$county, " County, ", input$state)) +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1))
    })
    
    output$sex_age_plot <- renderPlot({
        ggplot(filter(sex_age,
                      State == input$state,
                      County == input$county)) +
            geom_col(aes(x = Sex, y = estimate, fill = Age),
                     position = "dodge") +
            facet_grid(~ HI_Coverage) +
            labs(x = "Gender",
                 y = "Number of People", 
                 title = str_c("Age by Gender by Health Insurance Coverage in ", input$county, " County, ", input$state))
    })
    
    output$health_public_plot <- renderPlot({
        ggplot(filter(health_public,
                      State == input$state,
                      County == input$county)) +
            geom_col(aes(x = Sex, y = estimate, fill = Age),
                     position = "dodge") +
            facet_grid(~ Public_HI) + 
            labs(x = "Gender",
                 y = "Number of People",
                 title = str_c("Age by Gender by Coverage from Public Health Insurance in ", input$county, " County, ", input$state))
    })
    
    output$health_private_plot <- renderPlot({
        ggplot(filter(health_private,
                      State == input$state,
                      County == input$county)) +
            geom_col(aes(x = Sex, y = estimate, fill = Age),
                     position = "dodge") +
            facet_grid(~ Private_HI) + 
            labs(x = "Gender",
                 y = "Number of People",
                 title = str_c("Age by Gender by Coverage from Private Health Insurance in ", input$county, " County, ", input$state))
    })
    
    output$map_rates <- renderLeaflet({
        if(input$rate %in% c("case_fatality", "death_rate", "case_rate")){
        rate <- switch(input$rate,
                       case_fatality = covid_map_data$case_fatality,
                       death_rate = covid_map_data$death_rate,
                       case_rate = covid_map_data$case_rate,
                       covid_map_data$case_fatality)
        pal_rates <- colorNumeric(palette = "viridis", domain = rate)
        leaflet(width = "100%") %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>% 
            addPolygons(data = st_transform(covid_map_data, crs = "+init=epsg:4326"),
                        popup = ~ str_c("<strong>", covid_map_data$County, ", ", covid_map_data$State_abb,
                                        "</strong><br /> Cases: ", covid_map_data$Cases,
                                        "<br /> Deaths: ", covid_map_data$Deaths,
                                        "<br /> Case Fatality: ", covid_map_data$case_fatality, " %",
                                        "<br /> Cases per capita: ", covid_map_data$case_rate,
                                        "<br /> Deaths per capita: ", covid_map_data$death_rate),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal_rates(rate)) %>%
            addLegend(data = st_transform(covid_map_data),
                      "bottomright",
                      pal = pal_rates,
                      values = ~ rate,
                      title = str_to_title(str_replace(input$rate, "_", " ")),
                      opacity = 1)
        } else if(input$rate == "Cases"){
            leaflet(width = "100%") %>% 
                addProviderTiles(provider = "CartoDB.Positron") %>% 
                addPolygons(data = st_transform(covid_map_data_sans_Cook, crs = "+init=epsg:4326"),
                            popup = ~ popup_msg,
                            stroke = FALSE,
                            smoothFactor = 0,
                            fillOpacity = 0.7,
                            color = ~ pal(Cases)) %>%
                addPolygons(data = st_transform(covid_map_data_Cook, crs = "+init=epsg:4326"),
                            popup = ~ popup_msg_Cook,
                            stroke = FALSE,
                            smoothFactor = 0,
                            fillOpacity = 0.7,
                            color = c("Black", "Red")) %>%
                addLegend(data = st_transform(covid_map_data_sans_Cook),
                          "bottomright",
                          pal = pal,
                          values = ~ Cases,
                          title = "Confirmed Cases",
                          opacity = 1) %>% 
                addLegend(data = st_transform(covid_map_data_Cook),
                          "bottomleft",
                          labels = str_c(covid_map_data_Cook$County, ", ", 
                                         covid_map_data_Cook$State_abb, " - ",
                                         covid_map_data_Cook$Cases, " Cases"),
                          colors = c("Black", "Red"),
                          title = "Outliers",
                          opacity = 1)
        } else if(input$rate == "Deaths"){
            leaflet(width = "100%") %>% 
                addProviderTiles(provider = "CartoDB.Positron") %>% 
                addPolygons(data = st_transform(covid_map_data_sans_Cook, crs = "+init=epsg:4326"),
                            popup = ~ popup_msg,
                            stroke = FALSE,
                            smoothFactor = 0,
                            fillOpacity = 0.7,
                            color = ~ pal_death_sans_Cook(Deaths)) %>%
                addPolygons(data = st_transform(covid_map_data_Cook, crs = "+init=epsg:4326"),
                            popup = ~ popup_msg_Cook,
                            stroke = FALSE,
                            smoothFactor = 0,                       
                            fillOpacity = 0.7,
                            color = c("Black", "Red")) %>%
                addLegend(data = st_transform(covid_map_data_sans_Cook),
                          "bottomright",
                          pal = pal_death_sans_Cook,
                          values = ~ Deaths,
                          title = "Deaths",
                          opacity = 1) %>% 
                addLegend(data = st_transform(covid_map_data_Cook),
                          "bottomleft",
                          labels = str_c(covid_map_data_Cook$County, ", ",
                                         covid_map_data_Cook$State_abb, " - ",
                                         covid_map_data_Cook$Deaths, " Deaths"),
                          colors = c("Black", "Red"),
                          title = "Outliers",
                          opacity = 1)
            }
        })
    
    output$map_gini <- renderLeaflet({
        covid_map_data %>% 
            st_transform(crs = "+init=epsg:4326") %>% 
            leaflet(width = "100%") %>% 
            addProviderTiles(provider = "CartoDB.Positron") %>% 
            addPolygons(popup = str_c("<strong>", covid_map_data$County, ", ", covid_map_data$State_abb,
                                      "</strong><br /> Gini Index: ", covid_map_data$Gini),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal_gini(Gini)) %>% 
            addLegend("bottomright",
                      pal = pal_gini,
                      values = ~ Gini,
                      title = "Gini Index",
                      opacity = 1)
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
