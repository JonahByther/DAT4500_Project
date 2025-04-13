#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

if(!require(shiny)) install.packages('shiny')
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(openintro)) install.packages('openintro')
if(!require(plotly)) install.packages('plotly')
temp_anomalies <- read.csv("country-level-monthly-temperature-anomalies.csv")

# Rounding to 2 decimal places to replicate DF
temp_anomalies_v2 <- temp_anomalies |>
  mutate(across(January:December, ~ round(.x, 2)))

#Tidying Data
temp_anomalies_v2 <- temp_anomalies |>
  pivot_longer(cols = c(January:December), names_to = "Month", values_to = "average_temp") 

#Defining Month Order to display plot correctly
month_levels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")

# Creating Color fill for bars and adding order
temp_anomalies_v2 <- temp_anomalies_v2 |>
  mutate(temp_color = ifelse(average_temp < 0, "red", "blue" ),
         Month = factor(Month, levels = month_levels))

#Funciton for plotting
season_bar_plot <- function(yr, yr2, season_name){
  
  season_months <- switch(season_name,
                          "Winter" = c("December", "January", "February"),
                          "Spring" = c("March", "April", "May"),
                          "Summer" = c("June", "July", "August"),
                          "Fall"   = c("September", "October", "November"),
                          "World"  = c("January","February", "March", "April", "May", "June", 
                                       "July", "August", "September", "October", "November", "December"))
  
  p <- temp_anomalies_v2 |>
    mutate(text = paste("Year:", Year, "</b>",
                        "<br>World:", average_temp, "(\u00B0C)")) |>
    filter(Year >= yr & Year <= yr2, Month %in% season_months) |>
    ggplot(aes(x = Year, y = average_temp, fill = temp_color)) +
    geom_col(aes(text = text)) +
    theme(legend.position = "none") +
    facet_wrap(~Month) +
    ylab("Average Temperature, measured in Celsius")
  
  ggplotly(p, tooltip = "text")
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seasonal Anomalies"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("yr",
                        "Select start and end year:",
                        min = 1940,
                        max = 2025,
                        value = c(1940, 2025),
                        step = 1,
                        sep = "")
        ,
        selectInput("season", "Select season:",
                    choices = c("Winter", "Spring", "Summer", "Fall", "World"),
                    selected = "Winter")),
        
        # Show a plot of the generated distribution
        mainPanel(
          h3(textOutput("dynamic_title")),
          h5("Calculated as difference between average surface temperature from the mean temperature of the same month during the period 1991-2020."),
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$dynamic_title <- renderText({
      req(input$season)
      paste("Temperature anomalies by month,", input$season)
    })
    output$distPlot <- renderPlotly({
       season_bar_plot(input$yr[1],input$yr[2], input$season)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
