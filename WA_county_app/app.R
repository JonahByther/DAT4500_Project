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
library(plotly)

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
    filter(!is.na(temp_diff)) |>  # <== ensures no NA
    mutate(temp_category = cut(temp_diff,
                               breaks = c(-Inf, -10, -5, 0, 5, 10, 15, Inf),
                               labels = c("<= -10", "-10 to -5", "-5 to 0", "0 to 5", "5 to 10", "10 to 15", "> 15"),
                               right = FALSE))
  
  
  WA_county_map_joined <- WA_county_map |> left_join(comparison_data, by = c("county" = "COUNTY"))
  WA_county_map_sf <- st_as_sf(WA_county_map_joined)
  
  color_palette <- c("<= -10" = "#08306B", "-10 to -5" = "#2171B5", "-5 to 0" = "#DEEBF7",
                     "0 to 5" = "#FEE0D2", "5 to 10" = "#FC9272", "10 to 15" = "#CB181D", "> 15" = "#67000D")
  
  WA_county_map_sf <- WA_county_map_sf |> 
    mutate(hover_text = paste0(
      "County: ", county, "<br>",
      "Avg Temp (", comparison_year, "): ", round(TAVG, 2), "°F<br>",
      "Avg Temp (", baseline_year, "): ", round(baseline_TAVG, 2), "°F<br>",
      "Temp Change: ", round(temp_diff, 2), "°F"
    ))
  
  p <- ggplot(WA_county_map_sf) +
    geom_sf(aes(fill = temp_category, text = hover_text), color = "white", size = 0.1) +
    scale_fill_manual(values = color_palette, name = "Temp Diff (°F)") +
    labs(
      title = if (selected_month == "All Months") {
        paste("Change in Avg Temp (Annual)", baseline_year, "vs", comparison_year)
      } else {
        paste("Change in Avg Temp (", selected_month, ")", baseline_year, "vs", comparison_year)
      }
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = "hover_text")
}

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
    summarise(TAVG = mean(TAVG, na.rm = TRUE)) |>
    arrange(Year)
  
  model <- lm(TAVG ~ Year, data = data)
  
  pred_1985 <- predict(model, newdata = data.frame(Year = 1985))
  pred_2024 <- predict(model, newdata = data.frame(Year = 2024))
  
  # Calculate the difference
  temp_change <- round(pred_2024 - pred_1985, 3)
  
  # Add hover text
  data <- data |>
    mutate(hover_text = paste("Year: ", Year, "<br>", "Avg Temp: ", round(TAVG, 2), "°F"))
  
  # Plot
  p <- ggplot(data, aes(x = Year, y = TAVG)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(aes(text = hover_text), color = "darkblue") +
    geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed") +
    labs(
      title = paste0(
        "Avg Temp Over Time in ", selected_county,
        if (!is.na(temp_change)) paste0(" (Change: ", temp_change, "°F)"),
        ""
      ),
      y = "Average Temp (°F)",
      x = "Year"
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
}





# UI
ui <- fluidPage(
  titlePanel("Washington Temperature Visualization"),
  
  tabsetPanel(id = "tab",
              tabPanel("County Map Comparison",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("year_range", "Select a year range:",
                                       min = 1985, max = 2024, value = c(1985, 2000), step = 1, sep = ""),
                           selectInput("month", "Select a month:", choices = c("All Months", month.name), selected = "January")
                         ),
                         mainPanel(
                           plotlyOutput("changePlot")
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
                           plotlyOutput("linePlot")
                         )
                       )
              )
  )
)

# Server
server <- function(input, output) {
  output$changePlot <- renderPlotly({
    county_year(input$year_range[1], input$year_range[2], input$month)
  })
  
  output$linePlot <- renderPlotly({
    line_graph(input$line_county, input$line_month)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
