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
library(car)
library(apaTables)
library(DT)
library(psych)
library(factoextra)

#-------------------------- Seasonal Anomalies Code - START --------------------------#

cleaned_month_anomalies <- read.csv("cleaned_month_anomalies.csv")

#Function for plotting
season_bar_plot <- function(yr, yr2, season_name){
  
  season_months <- switch(season_name,
                          "Winter" = c("December", "January", "February"),
                          "Spring" = c("March", "April", "May"),
                          "Summer" = c("June", "July", "August"),
                          "Fall"   = c("September", "October", "November"),
                          "All Seasons"  = c("January","February", "March", "April", "May", "June", 
                                             "July", "August", "September", "October", "November", "December"))
  
  p <- cleaned_month_anomalies|>
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

#-------------------------- Seasonal Anomalies Code - END --------------------------#

#-------------------------- Annual Temp Anomalies Map - START --------------------------#

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
    mutate(text = paste("<b>",Entity,"</b>\n",Year,"</b>\n",round(temperature_anomaly, 2), "</b>°C")) |>
    
    ggplot() +
    geom_sf(aes(fill=temperature_anomaly, text=text), color="black") +
    scale_fill_continuous("", low="blue", high="red") +
    #labs(title = "Annual temperature anomalies, 2024.\nThe difference between a year's average surface temperature from the\n1991-2020 mean, in degrees Celcius.") +
    theme(legend.position = c(0.9, 0)) +
    my_map_theme()
  
  ggplotly(plotlyWTA_24, tooltip = "text") |>
    style(hoveron = "fill") 
}

#-------------------------- Annual Temp Anomalies Map - END --------------------------#

#-------------------------- WA County Significance - Start -------------------------#

cleaned_county_temp <- read.csv("cleaned_county_temp.csv")

# Define month levels in order
month_levels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")

# Ensure Month is a factor with correct order
cleaned_county_temp$Month <- factor(cleaned_county_temp$Month, levels = month_levels)

county_significance <- cleaned_county_temp |>
  mutate(text = paste0("Category: ", Category,
                       "\nMonth: ", Month, 
                       "\nCounties: ", Count)) |>
  ggplot(aes(x = Month, y = Count, fill = Category)) +
  geom_col(aes(text = text)) +
  scale_fill_manual(
    values = c(
      "Positive Slope Counties" = "#FF0000",
      "Not Significant" = "#808080",
      "Negative Slope Counties" = "#0000FF"
    )
  )+
  labs(
    x = "Month",
    y = "Number of Counties",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(hjust = 0.5)
  ) +
  annotate("text", x = "July", y = 42, label = "<b>Summer months have high significance with positive slope") +
  geom_segment(aes(x = 6.5, y = 40, xend = "June", yend = 35), 
               arrow = arrow()) +
  geom_segment(aes(x = 7, y = 40, xend = "July", yend = 35),
               arrow = arrow()) +
  geom_segment(aes(x = 7.5, y = 40, xend = "August", yend = 35),
               arrow = arrow())
  
  
  ggplotly(county_significance, tooltip = "text")
  
#-------------------------- WA County Significance - END -------------------------#

#-------------------------- Ocean and Temp Regression - START ------------------#

joined_global_data <- read.csv("joined_global_data.csv")
cleaned_wa_pca <- read.csv("cleaned_wa_pca.csv")

# Anomaly Ocean Line Chart
anom_ocean_heat_df <- joined_global_data |>
  mutate(text = paste0("Temperature Anomaly: ", round(Temperature_anomaly, 2), " (\u00B0C)",
                       "\nOcean Heat Content: ", round(Ocean_Heat, 2), " (10<sup>22</sup> Joules)",
                       "\nYear : ", Year))

anom_ocean_heat <- anom_ocean_heat_df |>
  ggplot(aes(x = Temperature_anomaly, y = Ocean_Heat)) + 
  geom_point(aes(text = text)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(
       subtitle = "Temperature Anomaly: the difference between a year's average \nsurface temperature from the 1991-2020 mean, in degrees Celsius. \nOcean heat is the top 700 meters of the oceans.") +
  ylab("Ocean Heat Content (10^22 Joules)") +
  xlab("Temperature Anomaly") +
  theme_minimal()
  
#Sea Surface Temp and Rad force Regression Graph
sea_rad_regression <- cleaned_wa_pca |> 
  ggplot(aes(x = Year, y = Temp)) +
  geom_line(linewidth = .5) +
  geom_smooth(method = "lm", color = "orange") +
  geom_point(aes(color = Sea_Surface_Temp_Anomaly, size = Total_Rad_Force)) +
  geom_point(aes(size = Total_Rad_Force), shape = 1, stroke = .85) +
  scale_color_gradientn(colors = c("yellow", 'red', "red4"), name = "Sea Surface\nTemperature\nAnomaly (°F)") +
  scale_size(range = c(1,5), name = "Global Radiative\nForcing (W/m^2)") +
  labs(
    #title = "Significant Predictors of Washington's Average Annual Temperature",
    #subtitle =
     # "Visualizing the impacts of sea surface temperature and global radiative forcing on Washington's
#temperature over time",
    #caption = 
     # "Sea Surface Temperature Anomaly is calculated by comparing each year's global average sea surface temperature \n     to the average surface temperature from 1971-2000.\nRadiative forcing measures the difference between energy (in the form of radiation) entering the atmosphere and leaving\n     the atmosphere. Positive values indicate that there is more energy entering Earth, resulting in warming.",
    y = "Temperature (°F)"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )

#Regression Models

Combo_model <- lm(Temp ~ Year + Sea_Surface_Temp_Anomaly + Total_Rad_Force, data = cleaned_wa_pca)

ghg_multi_sig <- lm(Temperature_anomaly ~ Year + Ocean_Heat + Total_Rad_Force, data = joined_global_data)
summary(ghg_multi_sig)

#-------------------- PCA Anomaly Code START ------------------#

#Removing all non numerical data and unecessary variables
global_numerical_data <- joined_global_data |>
  select(Year, Sea_Surface_Temp_Anomaly, Total_Rad_Force)

#Normalizing Data
global_normalized_data <- scale(global_numerical_data)

#Applying PCA
pca_global <- princomp(global_normalized_data)
summary(pca_global) 

#Loading matrix
pca_global$loadings[, 1:3]

#PCA regression
anom_pca_regression <- lm(Temperature_anomaly ~ pca_global$scores, data = joined_global_data)

#-------------------- PCA Anomaly Code START ------------------#

#-------------------- PCA WA State Code START ------------------#

datCombo <- cleaned_wa_pca |> 
  select(Year, Sea_Surface_Temp_Anomaly, Total_Rad_Force)
normCombo <- scale(datCombo)

data.pca <- princomp(normCombo)

data.pca$loadings[, 1:3]

pcModel <- lm(Temp ~ data.pca$scores, data = cleaned_wa_pca)

#-------------------- PCA WA State Code START ------------------#

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Climate Change Dashboard"), 
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Global Anomalies", tabName = "anomalies_tab", icon = icon("globe")),
      menuItem("Washington Counties Analysis", tabName = "wa_counties_tab", icon = icon("map")),
      menuItem("Multiple Regression Results", tabName = "ocean_regression_tab", icon = icon("table")),
      menuItem("Principal Components Analysis", tabName = "pca_tab", icon = icon("table")),
      menuItem("PCA Visualizations", tabName = "pca_visual_tab", icon = icon("table"))
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
      fluidRow(htmlOutput("seasons_caption"))
      ),
      box(title = "Annual Anomalies - World Map", solidHeader = TRUE,
          collapsible = TRUE, width = 12, background = "olive",
          selectInput("TempYears", "Select Year for Global Map:",
                      choices = sort(unique(AnnTempAnomolies$Year)),
                      selected = 2024),
          plotlyOutput("anomalies_mapPlot"),
          fluidRow(htmlOutput("anomalies_caption"))
          ),
    )
    ),
    tabItem(tabName = "wa_counties_tab",
            fluidRow(
              box(
                title = "Washington Monthly Regression Summary Across Counties", solidHeader = TRUE,
                collapsible = TRUE, width = 12, background = "olive",
                plotlyOutput("county_barplot"),
                fluidRow(htmlOutput("county_caption"))
              )
            )
    ),
    tabItem(tabName = "pca_tab",
            fluidRow(
              box(
                title = "Anomalies Principal Components Analysis", solidHeader = TRUE,
                collapsible = TRUE, width = 6, background = "olive",
                tags$h4("PCA Loadings"),
                verbatimTextOutput("pca_anomaly_loading"),
                tags$h4("PCA Anomaly Regression"),
                verbatimTextOutput("pca_anomaly"),

              ),
              box(
                title = "WA Temperature Principal Components Analysis", solidHeader = TRUE,
                collapsible = TRUE, width = 6, background = "olive",
                tags$h4("PCA Loadings"),
                verbatimTextOutput("pca_wa_temp_loading"),
                tags$h4("PCA WA Temperature Regression"),
                verbatimTextOutput("pca_wa_temp")
              )
              ),
            fluidRow(
              box(title = "Interpretation of Anomalies PCA", solidHeader = TRUE,
                  collapsible = TRUE, background = "blue", "For component 1, all variables are increasing at the same rate. Anomaly goes up when Year, Total Rad, and Sea surface temp anomaly increase. 
                    This is shown where all of component one have the same relative positive loading and coefficient of regression is positive. 
                    For component 2, Year and Rad increase. Sea surface temp anomaly decreases. Strong anomaly decrease shows as component increases sea temp anomaly will decrease and vice versa. 
                    Higher component 2 means higher rad force and ppm as time goes, but lower sea temp. The lagging of sea temp means a overall decrease in temp anomaly."
                    ),
            
            box(title = "Interpretation of WA PCA", background = "blue",
                "Interpretation goes here"))
    ),
    
    tabItem(tabName = "pca_visual_tab",
            fluidRow(
              box(title = "Scree Plots", solidHeader = TRUE,
                  collapsible = TRUE, background = "olive",
                  tags$h4("Global Anomalies"),
                  plotOutput("scree_anomalies"),
                  tags$h4("Washington Temperatures"),
                  plotOutput("scree_wa")
                  ),
              box(title = "Biplot of Attributes", solidHeader = TRUE,
                  collapsible = TRUE, background = "olive",
                  tags$h4("Global Anomalies"),
                  plotOutput("bi_plot_anom"),
                  tags$h4("Washington Temperature"),
                  plotOutput("bi_plot_wa"))
              
            )
            ),
    tabItem(tabName = "ocean_regression_tab",
            fluidRow(
              box(
                title = "Temperature Anomaly vs. Ocean Heat", solidHeader = TRUE, 
                collapsible = TRUE, width = 8, background = "olive",
                plotlyOutput("ocean_anom_graph"),
                fluidRow(htmlOutput("ocean_anom_caption"))
              ),
              box(
                title = "Temperature Anomaly Multi Regression", solidHeader = TRUE,
                collapsible = TRUE, width = 4, background = "olive",
                verbatimTextOutput("anom_multi_regression")
              ),
              box(
                title = "Significant Predictors of Washington's Average Annual Temperature", solidHeader = TRUE, 
                collapsible = TRUE, width = 8, background = "olive",
                plotOutput("sea_rad_regression_graph"),
                fluidRow(htmlOutput("sea_rad_regression_caption"))
              ),
              box(
                title = "Washington Temperature Multi Regression", solidHeader = TRUE, collapsible = TRUE,
                width = 4, background = "olive",
                verbatimTextOutput("combined_regression")
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
  
  output$sea_rad_regression_graph <- renderPlot({
    sea_rad_regression
  })
  
  output$seasons_caption <- renderUI({
    HTML("<font size='2px;'>&ensp;&ensp;&ensp;Temperature anomaly calculated as difference of specific month average surface temperature from the 1991-2020 mean
          <br>&ensp;&ensp;&ensp;<strong>Source:</strong>  <a href='https://ourworldindata.org/grapher/global-temperature-anomalies-by-month' style='color:#FFFFFF; text-decoration: underline;'> Our World in Data</a></font></p>")
  })
  
  output$anomalies_caption <- renderText({
    paste("<font size='2px;'>&ensp;&ensp;&ensp;Temperature anomaly calculated as difference between a year's average surface temperature from the 1991-2020 mean
          <br>&ensp;&ensp;&ensp;<strong>Source:</strong>  <a href='https://ourworldindata.org/grapher/annual-temperature-anomalies' style='color:#FFFFFF; text-decoration: underline;'> Our World in Data</a></font></p>")
    })
  
  output$county_caption <- renderText({
    paste("<font size='2px;'>&ensp;&ensp;&ensp;Summer months are defined as June-August. However, we do see high positive significant slopes in May and September as well. 
    <br>&ensp;&ensp;&ensp;Positive slope refers to a county's slope that is increasing in temperature, or getting hotter. 
          Negative slope refers to a slope that is decreasing in temperature, or getting colder.
          <br>&ensp;&ensp;&ensp;<strong>Source:</strong> <a href='https://www.ncei.noaa.gov/cdo-web/search' style='color:#FFFFFF; text-decoration: underline;'> National Atmospheric and Atmospheric Administration</a></font></p>")
  })
  
  output$pca_anomaly <- renderPrint({
    
    summary(anom_pca_regression)
    
  })
  
  output$pca_wa_temp <- renderPrint({
    
    summary(pcModel)
  })
  
  output$pca_anomaly_loading <- renderPrint({
    pca_global$loadings[, 1:3]
  })
  
  output$scree_anomalies <- renderPlot({
    fviz_eig(pca_global, addlabels = TRUE)
  })
  
  output$scree_wa <- renderPlot({
    fviz_eig(data.pca, addlabels = TRUE)
  })
  
  output$bi_plot_anom <- renderPlot({
    fviz_pca_var(pca_global, col.var = "black", repel = TRUE) +
      xlim(0, 1.2)
  })
  
  output$bi_plot_wa <- renderPlot({
    fviz_pca_var(data.pca, col.var = "black", repel = TRUE) +
      xlim(0, 1.2)
  })
  
  output$pca_wa_temp_loading <- renderPrint({
    data.pca$loadings[, 1:3]
  })
  output$ocean_anom_caption <- renderText({
    paste("<font size='2px;'>
        &ensp;&ensp;&ensp;Temperature Anomaly: the difference between a year's average surface temperature from the 1991–2020 mean, in degrees Celsius.
        <br>&ensp;&ensp;&ensp;Ocean heat is the top 700 meters of the oceans.
        <br>&ensp;&ensp;&ensp;<strong>Sources:</strong>
        <br>&ensp;&ensp;&ensp;<a href='https://gml.noaa.gov/aggi/aggi.html' style='color:#FFFFFF; text-decoration: underline;'>NOAA – AGGI Emissions Data</a>
        <br>&ensp;&ensp;&ensp;<a href='https://www.epa.gov/climate-indicators/climate-change-indicators-sea-surface-temperature' style='color:#FFFFFF; text-decoration: underline;'>EPA – Sea Surface Temperature</a>
        <br>&ensp;&ensp;&ensp;<a href='https://www.epa.gov/climate-indicators/climate-change-indicators-ocean-heat' style='color:#FFFFFF; text-decoration: underline;'>EPA – Ocean Heat</a>")
  })
  
  output$sea_rad_regression_caption <- renderText({
    paste("<font size='2px;'>&ensp;&ensp;&ensp;Sea Surface Temperature Anomaly is calculated by comparing each year's global average sea surface temperature to the average  
    <br>&ensp;&ensp;&ensp;&ensp;surface temperature from 1971-2000. Radiative forcing measures the difference between energy (in the form of radiation) entering
    <br>&ensp;&ensp;&ensp;the atmosphere and leaving the atmosphere. Positive values indicate that there is more energy entering Earth,
    <br>&ensp;&ensp;&ensp;&ensp;resulting in warming temperature over time.
    <br>&ensp;&ensp;&ensp;<strong>Sources:</strong>
        <br>&ensp;&ensp;&ensp;<a href='https://gml.noaa.gov/aggi/aggi.html' style='color:#FFFFFF; text-decoration: underline;'>NOAA – AGGI Emissions Data</a>
        <br>&ensp;&ensp;&ensp;<a href='https://www.epa.gov/climate-indicators/climate-change-indicators-sea-surface-temperature' style='color:#FFFFFF; text-decoration: underline;'>EPA – Sea Surface Temperature</a>
        <br>&ensp;&ensp;&ensp;<a href='https://www.epa.gov/climate-indicators/climate-change-indicators-ocean-heat' style='color:#FFFFFF; text-decoration: underline;'>EPA – Ocean Heat</a>") 
  })
  
  output$combined_regression <- renderPrint({
    summary(Combo_model)
  })
  
  output$anom_multi_regression <- renderPrint({
    summary(ghg_multi_sig)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
