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
library(openintro)
library(plotly)
library(shiny)
library("rnaturalearth")
library("rnaturalearthdata")
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
    theme(legend.position = c(0.9, 0), legend.direction = "horizontal") +
    my_map_theme()
  
  ggplotly(plotlyWTA_24, tooltip = "text") |>
    style(hoveron = "fill") 
}


ui <- fluidPage(
  
  # Application title
  titlePanel("Annual Temp Anomalies"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("TempYears",
                  "Year:",
                  min = 1940,
                  max = 2024,
                  value = 2024,
                  sep = "")
    ), mainPanel(h3(textOutput("titletext1")),
                 h4("The difference between a year's average surface temperature from the 1991-2020 mean, in degrees Celsius."),
                 plotlyOutput("mapPlot"),
                 h4(tags$a(href="https://ourworldindata.org/grapher/annual-temperature-anomalies", 
                           "Our World in Data annual temperature anomalies graph"), "With data retrieved from",
                    tags$a(href="https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=overview", 
                           "Copernicus Climate Change Service information - ERA5 monthly averaged data on single levels from 1940 to present."))
    )    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$titletext1 <- renderText(paste("Annual temperature anomalies,", input$TempYears))
  output$mapPlot <- renderPlotly({
    function1(TempYears = input$TempYears)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
