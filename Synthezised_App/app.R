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
library(usmap)
library(lubridate)
library(sf)

### Seasonal Anomalies Code - Start

temp_anomalies <- read.csv("country-level-monthly-temperature-anomalies.csv")

# Rounding to 2 decimal places to replicate DF
temp_anomalies_v2 <- temp_anomalies |>
  mutate(across(January:December, ~ round(.x, 2)))

#Tidying Data
temp_anomalies_v2 <- temp_anomalies_v2 |>
  pivot_longer(cols = c(January:December), names_to = "Month", values_to = "average_temp") 

#Defining Month Order to display plot correctly
month_levels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")

# Creating Color fill for bars and adding order
temp_anomalies_v2 <- temp_anomalies_v2 |>
  mutate(temp_color = ifelse(average_temp < 0, "red", "blue" ),
         Month = factor(Month, levels = month_levels))

#Function for plotting
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


### WA County Code - Start

WA_county <- read.csv("WA_county.csv")

us_county <- us_map(region = "counties")
WA_county_map <- us_county |>
  filter(full == "Washington")

# Function to create the county map
county_year <- function(baseline_year, comparison_year, selected_month) {
  month_num <- match(selected_month, month.name)
  
  if (selected_month == "All Months") {
    baseline_data <- WA_county |>
      filter(year(DATE) == baseline_year) |> group_by(COUNTY) |> summarise(baseline_TAVG = mean(TAVG, na.rm = TRUE))
    
    selected_data <- WA_county |>
      filter(year(DATE) == comparison_year) |> group_by(COUNTY) |> summarise(TAVG = mean(TAVG, na.rm = TRUE))
    
  } else {
    baseline_data <- WA_county |>
      filter(year(DATE) == baseline_year, month(DATE) == month_num) |>
      group_by(COUNTY) |> summarise(baseline_TAVG = mean(TAVG, na.rm = TRUE))
    
    selected_data <- WA_county |> filter(year(DATE) == comparison_year, month(DATE) == month_num) |> group_by(COUNTY) |> summarise(TAVG = mean(TAVG, na.rm = TRUE))
  }
  
  comparison_data <- selected_data |>
    left_join(baseline_data, by = "COUNTY") |>
    mutate(temp_diff = TAVG - baseline_TAVG) |>
    mutate(temp_category = cut(temp_diff,
                               breaks = c(-Inf, -10, -5, 0, 5, 10, 15, Inf),
                               labels = c("<= -10", "-10 to -5", "-5 to 0", "0 to 5", "5 to 10", "10 to 15", "> 15"),
                               right = FALSE))
  
  WA_county_map_joined <- WA_county_map |> left_join(comparison_data, by = c("county" = "COUNTY"))
  WA_county_map_sf <- st_as_sf(WA_county_map_joined)
  
  color_palette <- c("<= -10" = "#08306B", "-10 to -5" = "#2171B5", "-5 to 0" = "#DEEBF7",
                     "0 to 5" = "#FEE0D2", "5 to 10" = "#FC9272", "10 to 15" = "#CB181D", "> 15" = "#67000D")
  
  ggplot(WA_county_map_sf) +
    geom_sf(aes(fill = temp_category), color = "white", size = 0.1) +
    scale_fill_manual(values = color_palette, name = "Temp Diff (°F)") +
    labs(
      title = if (selected_month == "All Months") {
        paste("Change in Avg Temp (Annual)", baseline_year, "vs", comparison_year)
      } else {
        paste("Change in Avg Temp (", selected_month, ")", baseline_year, "vs", comparison_year)
      },
      fill = "Temp Diff (°F)"
    ) +
    theme_minimal()
}

# Function to generate the line graph
line_graph <- function(selected_county, selected_month) {
  month_num <- match(selected_month, month.name)
  
  data <- WA_county |>
    filter(COUNTY == selected_county) |>
    mutate(Year = year(DATE))
  
  # If a specific month is selected, filter by month
  if (selected_month != "All Months") {
    data <- data |> filter(month(DATE) == month_num)
  }
  
  data <- data |>
    group_by(Year) |>
    summarise(TAVG = mean(TAVG, na.rm = TRUE))
  
  ggplot(data, aes(x = Year, y = TAVG)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "darkblue") +
    labs(
      title = paste("Avg Temp Over Time in", selected_county),
      y = "Average Temp (°F)",
      x = "Year"
    ) +
    theme_minimal()
}

### WA County Code - End

# UI
ui <- fluidPage(
  titlePanel("Climate Change Visualization"),
  
  tabsetPanel(id = "tab",
              tabPanel("County Map Comparison",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("year_range", "Select a year range:",
                                       min = 1985, max = 2024, value = c(1985, 2000), step = 1, sep = ""),
                           selectInput("month", "Select a month:", choices = c("All Months", month.name), selected = "January")
                         ),
                         mainPanel(
                           plotOutput("changePlot")
                         )
                       )
              ),
              
              tabPanel("Line Chart Over Time",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("line_county", "Select County:",
                                       choices = sort(unique(WA_county$COUNTY)), selected = "King"),
                           selectInput("line_month", "Select Month:",
                                       choices = c("All Months", month.name), selected = "All Months")
                         ),
                         mainPanel(
                           plotOutput("linePlot")
                         )
                       )
              ),
              tabPanel("Seasonal Anomalies",
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
                         mainPanel(
                           plotlyOutput("seasonPlot")
                         )
                       )
              )
  )
)



# Server
server <- function(input, output) {
  
  output$changePlot <- renderPlot({
    county_year(input$year_range[1], input$year_range[2], input$month)
  })
  
  output$linePlot <- renderPlot({
    line_graph(input$line_county, input$line_month)
  })
  
  output$seasonPlot <- renderPlotly({
    season_bar_plot(input$yr[1], input$yr[2], input$season)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
