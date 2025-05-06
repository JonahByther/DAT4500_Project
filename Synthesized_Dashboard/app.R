#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(openintro)
library(plotly)
library(usmap)
library(dplyr)
library(lubridate)
library(sf)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)
library(supernova)
library(stats)
library(scales)
library(readxl)
library(dplyr)

### Seasonal Anomalies Code - START

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
                          "All Seasons"  = c("January","February", "March", "April", "May", "June", 
                                             "July", "August", "September", "October", "November", "December"))
  
  p <- temp_anomalies_v2 |>
    mutate(text = paste("Year:", Year, "</b>",
                        "<br>World:", average_temp, "(\u00B0C)")) |>
    filter(Year >= yr & Year <= yr2, Month %in% season_months) |>
    ggplot(aes(x = Year, y = average_temp, fill = temp_color)) +
    geom_col(aes(text = text)) +
    theme(legend.position = "none") +
    facet_wrap(~Month) +
    ylab("Anomaly, measured in Celsius")
  
  ggplotly(p, tooltip = "text")
  
}

### Annual Temperature Anomalies Map Code - START

AnnTempAnomolies <- read.csv("annual-temperature-anomalies.csv")

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(sovereignt != "Antarctica")

function1 <-function(TempYears) {
  
  TempAnomalies_24 <- AnnTempAnomolies |>
    filter(Year == TempYears)
  
  WorldTempAnom_24 <- world |>
    left_join(TempAnomalies_24, by = c("iso_a3_eh" = "Code"))
  
  my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
  }
  
  plotlyWTA_24 <- WorldTempAnom_24 |>
    mutate(text = paste("<b>",Entity,"</b>\n",Year,"</b>\n",temperature_anomaly,"</b>°C")) |>
    
    ggplot() +
    geom_sf(aes(fill=temperature_anomaly, text=text), color="black") +
    scale_fill_continuous("", low="blue", high="red") +
    #labs(title = "Annual temperature anomalies, 2024.\nThe difference between a year's average surface temperature from the\n1991-2020 mean, in degrees Celcius.") +
    theme(legend.position = c(0.9, 0)) +
    my_map_theme()
  
  ggplotly(plotlyWTA_24, tooltip = "text") |>
    style(hoveron = "fill") 
}

### Annual Anomalies END

### Washington Emission Data Regression START

WA <- read.csv("WAmonth.csv", skip = 3) |> 
  separate(col = Date, into = c("Year", "Month"), sep = 4)|> 
  group_by(Year) |> 
  summarize (Temp = mean(Value)) |> 
  ungroup()
WA$Year <- as.numeric(WA$Year)

emissions <- read_excel("US Energy CO2 Emissions.xlsx", skip = 4) |> 
  filter(State == "Washington") |> 
  select(`1970`:`2022`) |> 
  pivot_longer(cols = `1970`:`2022`, names_to = "Year", values_to = "Emissions")
emissions$Year <- as.numeric(emissions$Year)

capita <- read_excel("energy_CO2_per_capita.xlsx", skip = 4) |> 
  filter(State == "Washington") |> 
  select(`1970`:`2022`) |> 
  pivot_longer(cols = `1970`:`2022`, names_to = "Year", values_to = "Per_Capita")
capita$Year <- as.numeric(capita$Year)

WA_emit <- emissions |> 
  left_join(capita, by = c("Year" = "Year")) |> 
  left_join(WA, by = c("Year" = "Year"))

AGGI <- read.csv("AGGI_Table.csv", skip = 2)

AGGI <- AGGI[-seq(nrow(AGGI),nrow(AGGI)-3),] |> 
  rename(Annual_PPM = Total,
         Cumulative_PPM = Total.1,
         Proportion_1990 = `X1990...1`,
         Change_Percent_Previous = `change...`,
         CFC = `CFC.`,
         HFCs = `HFCs.`)
AGGI$Year <- as.numeric(AGGI$Year)

AGGI_emit <- WA_emit |> 
  filter(Year >= 1979) |> 
  left_join(AGGI, by = c("Year" = "Year"))

  # Regression Data Summaries

emitSig <- lm(Emissions ~ Year, data = WA_emit)

capitaSig <- lm(Per_Capita ~ Year, data = WA_emit)

tempSig <-lm(Temp ~ Year, data = WA_emit)

### WA State Regression END

### WA Counties Analysis START

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
    filter(!is.na(temp_diff)) |>
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
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(p, tooltip = "hover_text")
}

# Function to generate the line graph
line_graph <- function(selected_county, selected_month) {
  month_num <- match(selected_month, month.name)
  
  data <- WA_county |>
    filter(COUNTY == selected_county & year(DATE) != 2025) |>
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
  p_value <- summary(model)$coefficients["Year", "Pr(>|t|)"]
  
  pred_1975 <- predict(model, newdata = data.frame(Year = 1975))
  pred_2024 <- predict(model, newdata = data.frame(Year = 2024))
  
  # Calculate the difference
  temp_change <- round(pred_2024 - pred_1975, 3)
  
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
        if (!is.na(temp_change)) paste0(" (Change per year: ", p_value, "°F)"),
        ""
      ),
    ) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = "text")
}
### WA Counties Analysis END



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Climate Change Dashboard"), 
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Global Anomalies", tabName = "anomalies_tab", icon = icon("globe")),
      menuItem("Washington State Regression", tabName = "wa_state_tab", icon = icon("line-chart")),
      menuItem("Regression Results", tabName = "regression_summary_tab", icon = icon("table")),
      menuItem("Washington Counties Analysis", tabName = "wa_counties_tab", icon = icon("map")))
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "anomalies_tab",
    fluidRow(
      box(
        title = "Seasonal Anomalies", solidHeader = TRUE, 
        collapsible = TRUE, width = 12, background = "olive",
      sliderInput("yr", "Select Years for Season Map:",
                  min = min(temp_anomalies_v2$Year),
                  max = max(temp_anomalies_v2$Year),
                  value = c(2000, 2020),
                  sep = ""),
      selectInput("season", "Select Season:",
                  choices = c("Winter", "Spring", "Summer", "Fall", "All Seasons"),
                  selected = "Winter"),
        plotlyOutput("seasonPlot", height = 250),
      ),
      box(title = "Annual Anomalies - World Map", solidHeader = TRUE,
          collapsible = TRUE, width = 12, background = "olive",
          selectInput("TempYears", "Select Year for Global Map:",
                      choices = sort(unique(AnnTempAnomolies$Year)),
                      selected = 2024),
          plotlyOutput("anomalies_mapPlot"))
    )
    ),
    tabItem(tabName = "wa_state_tab",
            fluidRow(
              box(
                title = "Washington CO2 Emissions Analysis",
                solidHeader = TRUE, collapsible = TRUE, background = "olive", width = 12,
              tabBox(
                id = "tabset1", width = 12,
                tabPanel("Annual Total Energy-Related Carbon Emissions", plotOutput("wa_emit_total")),
                tabPanel("Energy-Related CO2 Emissions Per Capita", plotOutput("wa_emit_percapita"))
              )
              ),
              box(
                title = "Washington State's Annual Average temperature", solidHeader = TRUE,
                collapsible = TRUE, width = 6, background = "olive",
                plotOutput("wa_avg_temp")
              ),
              box(
                title = "Cumulative Parts Per Million (PPM)", solidHeader = TRUE,
                collapsible = TRUE, width = 6, side = "right", background = "olive",
                plotOutput("wa_ppm")
              )
            )
    ),
    tabItem(tabName = "regression_summary_tab",
            fluidRow(
              box(
                title = "Washington State Annual Energy-Related Carbon Emissions Results",
                solidHeader = TRUE, collapsible = TRUE, background = "olive",
                verbatimTextOutput("emit_summary")
              ),
              box(
                title = "Washington State Energy-Related Carbon Emissions Per Capita",
                solidHeader = TRUE, collapsible = TRUE, background = "olive",
                verbatimTextOutput("emit_per_capita_summary")
              ),
              box(
                title = "Washington State Annual Average Temperature Over Time",
                solidHeader = TRUE, collapsible = TRUE, background = "olive", width = 12,
                verbatimTextOutput("temp_summary")
              )
            )
    ),
    tabItem(tabName = "wa_counties_tab",
            fluidRow(
              box(
                title = "Washington County Map Average Temperature Comparison", solidHeader = TRUE,
                collapsible = TRUE, width = 12, background = "olive",
                sliderInput("year_range", "Select a year range:",
                            min = 1975, max = 2024, value = c(1985, 2000), step = 1, sep = ""),
                selectInput("month", "Select a month:", choices = c("All Months", month.name), selected = "January"),
                plotlyOutput("county_mapplot")
              )
            )
    )
  )
)
)
    

    

  

  

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$seasonPlot <- renderPlotly({
    season_bar_plot(input$yr[1], input$yr[2], input$season)
  })
  
  output$anomalies_mapPlot <- renderPlotly({
    function1(TempYears = input$TempYears)
  })
  
  output$wa_emit_total <- renderPlot({
    WA_emit |> 
      ggplot(aes(x= Year, y = Emissions)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      theme_classic() +
      scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
      labs(
        title = "Washington State's Annual Energy-Related CO2 Emissions Per Capita",
        y = "Million Metric Tons CO2"
      )
  })
  
  output$wa_emit_percapita <- renderPlot({
    WA_emit |> 
      ggplot(aes(x= Year, y = Per_Capita)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      theme_classic() +
      scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
      labs(
        title = "Washington State's Annual Energy-Related CO2 Emissions Per Capita",
        y = "Metric Tons of CO2 Per Person"
      )
  })
  
  output$wa_avg_temp <- renderPlot({
    WA |> 
      ggplot(aes(x= Year, y = Temp)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      theme_classic() +
      scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
      labs(
        title = "Washington State's Annual Average temperature",
        y = "Temperature (°F)"
      )
  })
  
  output$wa_ppm <- renderPlot({
    AGGI_emit |> 
      ggplot(aes(x= Year, y = Cumulative_PPM)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      theme_classic() +
      scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
      labs(
        title = "Cumulative PPM",
        y = "Greenhouse Gas PPM"
      )
  })
  
  output$county_mapplot <- renderPlotly({
    county_year(input$year_range[1], input$year_range[2], input$month)
  })
  
  output$emit_summary <- renderPrint({
    summary(emitSig)
  })
  
  output$emit_per_capita_summary <- renderPrint({
    summary(capitaSig)
  })
  
  output$temp_summary <- renderPrint({
    summary(tempSig)
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
