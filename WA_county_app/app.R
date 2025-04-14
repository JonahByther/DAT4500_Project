#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(usmap)
library(lubridate)
library(sf)
WA_county <- read.csv("WA_county.csv")

us_county <- us_map(region = "counties",)
WA_county_map <- us_county |>
  filter(full == "Washington")

county_year <- function(selected_year, selected_month) {
  # Convert month name to number, unless "All Months"
  month_num <- match(selected_month, month.name)
  
  if (selected_month == "All Months") {
    baseline_1985 <- WA_county |>
      filter(year(DATE) == 1985) |>
      group_by(COUNTY) |>
      summarise(baseline_TAVG = mean(TAVG, na.rm = TRUE))
    
    selected_data <- WA_county |>
      filter(year(DATE) == selected_year) |>
      group_by(COUNTY) |>
      summarise(TAVG = mean(TAVG, na.rm = TRUE))
    
  } else {
    baseline_1985 <- WA_county |>
      filter(year(DATE) == 1985, month(DATE) == month_num) |>
      group_by(COUNTY) |>
      summarise(baseline_TAVG = mean(TAVG, na.rm = TRUE))
    
    selected_data <- WA_county |>
      filter(year(DATE) == selected_year, month(DATE) == month_num) |>
      group_by(COUNTY) |>
      summarise(TAVG = mean(TAVG, na.rm = TRUE))
  }
  
  comparison_data <- selected_data |>
    left_join(baseline_1985, by = "COUNTY") |>
    mutate(temp_diff = TAVG - baseline_TAVG) |>
    mutate(
      temp_category = cut(
        temp_diff,
        breaks = c(-Inf, -10, -5, 0, 5, 10, 15, Inf),
        labels = c("<= -10", "-10 to -5", "-5 to 0", "0 to 5", "5 to 10", "10 to 15", "> 15"),
        right = FALSE
      )
    )
  
  WA_county_map_joined <- WA_county_map |>
    left_join(comparison_data, by = c("county" = "COUNTY"))
  
  WA_county_map_sf <- st_as_sf(WA_county_map_joined)
  
  color_palette <- c(
    "<= -10" = "#08306B",   
    "-10 to -5" = "#2171B5", 
    "-5 to 0" = "#DEEBF7",  
    "0 to 5" = "#FEE0D2",   
    "5 to 10" = "#FC9272",  
    "10 to 15" = "#CB181D", 
    "> 15" = "#67000D"     
  )
  
  ggplot(WA_county_map_sf) +
    geom_sf(aes(fill = temp_category), color = "white", size = 0.1) +
    scale_fill_manual(
      values = color_palette,
      name = "Temp Diff (°F)"
    ) +
    labs(
      title = if (selected_month == "All Months") {
        paste("Change in Avg Temp (Annual)", selected_year, "vs 1985")
      } else {
        paste("Change in Avg Temp (", selected_month, ") ", selected_year, "vs 1985")
      }
      ,
      fill = "Temp Diff (°F)"
    ) +
    theme_minimal()
}





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Year-to-Year Change in US Average Temperature"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Select a year:",
                  min = 1986,
                  max = 2024,
                  value = 1986,
                  ticks = FALSE,
                  animate = animationOptions(interval = 1500)
                  
      ),
      selectInput("month", 
                  "Select a month:", 
                  choices = c("All Months", month.name),  # Built-in month names
                  selected = "January")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("changePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$changePlot <- renderPlot({
    county_year(input$year, input$month)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
