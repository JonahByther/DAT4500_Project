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
if(!require(usmap)) install.packages('usmap')
if(!require(lubridate)) install.packages('lubridate')
if(!require(sf)) install.packages('sf')
if(!require(scales)) install.packages('scales')
if(!require(dplyr)) install.packages('dplyr')
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rnaturalearth)) install.packages("rnaturalearthdata")

WA_county <- read.csv("WA_county.csv")
temp_anomalies <- read.csv("country-level-monthly-temperature-anomalies.csv")
state_temp <- read_csv("avg_state_temps.csv") |> 
  select(-...1)

### WA County Code - START

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
    # labs(
    #   title = if (selected_month == "All Months") {
    #     paste("Change in Avg Temp (Annual)", baseline_year, "vs", comparison_year)
    #   } else {
    #     paste("Change in Avg Temp (", selected_month, ")", baseline_year, "vs", comparison_year)
    #   }
    # ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(p, tooltip = "hover_text")
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
        if (!is.na(temp_change)) paste0(" (Change per year: ", round(temp_change / 39, 3), "°F)"),
        ""
      ),
    ) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = "text")
}

### WA County Code - END


###US Map Code - START

# Create geometries for US map, create map theme and color scheme
us_states <- us_map()
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

map_data <- us_states |> 
  filter(abbr != "DC") |> 
  left_join(state_temp, by=c("abbr" = "state"))

change_cutpoints <- c(-20, -2.5, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 2.5, 20)
change_labels <- c("More than -2.5","-2.5 to -2", "-2 to -1.5", "-1.5 to -1",
                   "-1 to -.5", "-.5 to 0", "0 to .5", ".5 to 1", "1 to 1.5", 
                   "1.5 to 2", "2 to 2.5", "More than 2.5")
change_colors <- c("darkblue","#2166ac","#4393c3","#92c5de","#d1e5f0", "#eef4f7",
                   "#f7e7e4","#fddbc7","#f4a582","#d6604d","#b2182b", "red4")

interactive_us <- function(yr1) {
  # Rescale cutpoints to 0–1 for gradientn
  rescaled_vals <- rescale(change_cutpoints, to = c(0, 1))
  
  plot <- map_data |> 
    select(full, geom, paste("temp", yr1, sep = ""), temp1974) |> 
    rename(temp1 = paste("temp", yr1, sep = "")) |> 
    mutate(
      temp_diff = temp1 - map_data$temp1974,
      text = paste0(
        "<b>", full, "</b><br>",
        "Avg temp in ", yr1, ": <i>", signif(temp1, 3), "°F</i><br>",
        "<i>", signif(temp_diff, 3), "°F</i> ",
        ifelse(temp_diff >= 0, "hotter", "colder"), " than 1974"
      )
    ) |> 
    ggplot() +
    geom_sf(aes(fill = temp_diff, text = text, group = full), color = "black") +
    scale_fill_gradientn(
      colors = change_colors,
      values = rescaled_vals,
      limits = range(change_cutpoints),
      oob = scales::squish,
      name = ""
    ) +
    my_map_theme() +
    #labs(
    #  title = ifelse(yr1 < 1975,
    #               paste("Difference in Average Annual State Temp from", yr1, "to 1974"),
    #               paste("Difference in Average Annual State Temp from 1974 to", yr1)),
    #subtitle = "Visualizing how temperatures in the US have changed relative to 1974",
    #caption = "Data from NOAA's Global Summary of the Year (GSOY)"
    #) +
    theme(
      #plot.title = element_text(size = 24, hjust = .34,), #Original hjust = 0
      #plot.subtitle = element_text(size = 16, hjust = .27,), #Tweaked for recording, del hjust
      #plot.caption = element_text(size = 14, hjust = 2.5,), #Tweaked for recording, del hjust
      legend.position = "none"
    )
  
  ggplotly(plot, tooltip = "text") |>
    style(hoveron = "fill")
}

### US Map Code - END



### Seasonal Anomalies Code - START

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

### Season Anomalies Code - END

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Climate Change Visualization"),
    tabsetPanel(id = "tab",

    tabPanel("Seasonal Anomalies", value = "season_anomalies",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("yr",
                             "Select start and end year:",
                             min = 1940,
                             max = 2025,
                             value = c(1940, 2025),
                             step = 1,
                             sep = "",
                             ticks = FALSE)
                 ,
                 selectInput("season", "Select season:",
                             choices = c("Winter", "Spring", "Summer", "Fall", "All Seasons"),
                             selected = "Winter")),
               mainPanel(
                 h3(textOutput("TitleText_season"), align = "left"),
                 h5("Calculated as difference between average surface temperature from the mean temperature\n 
                              of the same month during the period 1991-2020."),
                 plotlyOutput("seasonPlot"),
                 h5("Data retrieved by World in Data, contains modified Copernicus Climate Change Service information (2025)")
               )
    )),
    tabPanel(title = "United States Map",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year",
                             "Select a year:",
                             min = 1924,
                             max = 2024,
                             value = 1924,
                             ticks = FALSE,
                             sep = ""
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 h3(textOutput("TitleText_US"), align = "center"),
                 h5("Displays the each state's average annual temperature and the difference
                  in temperatures between the selected year an 1974", align = "center"),
                 plotlyOutput("interactive_USPlot"),
                 h5("Data from NOAA's Global Summary of the Year (GSOY)", align = "right")
                 #img(src = "legendV2.png", width = "80%", align = "right")
               )
             )
    ),
    tabPanel("County Map Comparison",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_range", "Select a year range:",
                             min = 1985, max = 2024, value = c(1985, 2000), step = 1, sep = ""),
                 selectInput("month", "Select a month:", choices = c("All Months", month.name), selected = "January")
               ),
               mainPanel(
                 h3(textOutput("TitleText_map_county")),
                 plotlyOutput("county_mapPlot"),
                 h5("Data retrieved from National Oceanic and Atmospheric Association (NOAA)")
               )
             )
    ),
    tabPanel("County Temperature Over Time",
             sidebarLayout(
               sidebarPanel(
                 selectInput("line_county", "Select County:",
                             choices = sort(unique(WA_county$COUNTY)), selected = "King"),
                 selectInput("line_month", "Select Month:",
                             choices = c("All Months", month.name), selected = "All Months")
               ),
               mainPanel(
                 h3(textOutput("TitleText_line_county"), align = "middle"),
                 plotlyOutput("county_linePlot"),
                 h5("Data retrieved from National Oceanic and Atmospheric Association (NOAA)")
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
  
  output$TitleText_season <- renderText(
    paste(input$season, "Temperature Anomalies")
  )
  
  output$interactive_USPlot <- renderPlotly({
    interactive_us(input$year) 
  })
  
  output$TitleText_US <- renderText(
    ifelse(input$year < 1975,
           paste("Difference in Average Annual State Temp from", input$year, "to 1974"),
           paste("Difference in Average Annual State Temp from 1974 to", input$year))
  )
  
  output$county_mapPlot <- renderPlotly({
    county_year(input$year_range[1], input$year_range[2], input$month)
  })
  
  output$TitleText_map_county <- renderText(
    paste0("Change in Average Temperature from ", input$year_range[1], " to ", input$year_range[2], ", ", input$month) 
  )
  
  output$county_linePlot <- renderPlotly({
    line_graph(input$line_county, input$line_month)
  })
  
  output$TitleText_line_county <- renderText(
    paste0("Average Temperature Over Time in ", input$line_county, ", ", input$line_month)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
