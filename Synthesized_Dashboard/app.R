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
library(DT)

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

### WA County Significance
WA_county <- read.csv("WA_county.csv")

county_significance <- long_data |>
  mutate(text = paste0("Category: ", Category,
                       "\nMonth ", Month, 
                       "\nSignificant Counties: ", Count)) |>
  ggplot(aes(x = Month, y = Count, fill = Category)) +
  geom_col(aes(text = text)) +
  scale_fill_manual(
    values = c(
      "Positive_Slope_Counties" = "#FF0000",
      "Not_Significant" = "#808080",
      "Negative_Slope_Counties" = "#0000FF"
    )
  )+
  labs(
    x = "Month",
    y = "Number of Counties",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  
  ggplotly(county_significance, tooltip = "text")

### Global Temp and Sea Regression

emissions_oceans <- read.csv("emissions_oceans.csv")

annual_global_anomalies <- annual_anomalies |>
  filter(Entity == "World")

# Rounding to 2 decimal places to replicate DF

# Joining Data
joined_global_data <- emissions_oceans |>
  left_join(annual_global_anomalies, by = "Year")

joined_global_data <- joined_global_data |>
  rename(Temperature_anomaly = Temperature.anomaly)

# Visuals


anom_ocean_heat <- joined_global_data |>
  mutate(text = paste0("Temperature Anomaly: ", round(Temperature_anomaly, 2), " (\u00B0C)",
                       "\nOcean Heat Content: ", round(Ocean_Heat, 2), " (10<sup>22</sup> Joules)",
                       "\nYear : ", Year)) |>
  ggplot(aes(x = Temperature_anomaly, y = Ocean_Heat)) + 
  geom_point(aes(text = text)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Temperature Anomaly vs. Ocean Heat",
       subtitle = "Temperature Anomaly: the difference between a year's average \nsurface temperature from the 1991-2020 mean, in degrees Celsius. \nOcean heat is the top 700 meters of the oceans.") +
  ylab("Ocean Heat Content (10^22 Joules)") +
  xlab("Temperature Anomaly") +
  theme_minimal()


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Climate Change Dashboard"), 
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Global Anomalies", tabName = "anomalies_tab", icon = icon("globe")),
      menuItem("Washington Counties Analysis", tabName = "wa_counties_tab", icon = icon("map")),
      menuItem("Ocean Temperature Analysis", tabName = "ocean_tab", icon = icon("tint")),
      menuItem("Ocean Regression Results", tabName = "ocean_regression_tab", icon = icon("table"))
  )),
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
    tabItem(tabName = "wa_counties_tab",
            fluidRow(
              box(
                title = "Washington Monthly Regression Summary Across Counties", solidHeader = TRUE,
                collapsible = TRUE, width = 12, background = "olive",
                plotlyOutput("county_barplot")
              )
            )
    ),
    tabItem(tabName = "ocean_tab",
            fluidRow(
              box(
                title = "Ocean Analysis", solidHeader = TRUE,
                collapsible = TRUE, width = 12, status = "success",
                DT::dataTableOutput("emissions_ocean_table")
              ),
              box(title = "Observations", background = "blue", "Example Text that we can put for analysis paragraphs etc.")
            )
    ),
    tabItem(tabName = "ocean_regression_tab",
            fluidRow(
              box(
                solidHeader = TRUE,collapsible = TRUE, width = 6, background = "olive",
                plotlyOutput("ocean_anom_graph")
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
  

  output$county_barplot <- renderPlotly({
    
    ggplotly(county_significance, tooltip = "text")
    
  })
  
  output$ocean_anom_graph <- renderPlotly({
    
    ggplotly(anom_ocean_heat, tooltip = "text")
  })
  
  output$emissions_ocean_table <- DT::renderDataTable({
    DT::datatable(
      joined_global_data,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  output$emit_summary <- renderUI({
    
    tags$img(src = "Annual_Emission.png",
             style = "width:100%; height:auto;")
  })
  
  output$emit_per_capita_summary <- renderUI({
    
    tags$img(src = "Per_Capita.png",
             style = "width:100%; height:auto;")
  })
  
  output$temp_summary <- renderPrint({
    tags$img(src = "Average_Temp.png",
             style = "width:100%; height:auto;")
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
