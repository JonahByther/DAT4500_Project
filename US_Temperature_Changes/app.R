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
library(dplyr)
library(usmap)
library(scales)
library(plotly)

state_temp <- read_csv("avg_state_temps.csv") |> 
  select(-...1)

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

#change_cutpoints <- c(-20, -2.5, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 2.5, 20)
#change_labels <- c("More than -2.5","-2.5 to -2", "-2 to -1.5", "-1.5 to -1",
#                   "-1 to -.5", "-.5 to 0", "0 to .5", ".5 to 1", "1 to 1.5", 
#                   "1.5 to 2", "2 to 2.5", "More than 2.5")
#change_colors <- c("darkblue","#2166ac","#4393c3","#92c5de","#d1e5f0", "#eef4f7",
#                   "#f7e7e4","#fddbc7","#f4a582","#d6604d","#b2182b", "red4")

change_cutpoints <- c(-20, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 20)
change_labels <- c("More than -5","-5 to -4", "-4 to -3", "-3 to -2",
                   "-2 to -1", "-1 to 0", "0 to 1", "1 to 2", "2 to 3", 
                   "3 to 4", "4 to 5", "More than 5")
change_colors <- c("darkblue","#2166ac","#4393c3","#92c5de","#d1e5f0", "#eef4f7",
                   "#f7e7e4","#fddbc7","#f4a582","#d6604d","#b2182b", "red4")

interactive <- function(yr1) {
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


timelapse <- function(yr1) {
  map_data |> 
    select(full, geom, paste("temp", yr1, sep = ""), temp1974) |> 
    rename(
      temp1 = paste("temp", yr1, sep = ""),
    ) |> 
    mutate(
      temp_diff = temp1 - map_data$temp1974, 
      difference = cut(temp_diff, breaks = change_cutpoints, labels = change_labels)
    ) |> 
    ggplot() +
    geom_sf(aes(fill = difference)) +
    scale_fill_manual("Change in Temperature (F°)", values = change_colors, drop = F) +
    my_map_theme() +
    labs(
      title = ifelse(yr1 < 1975,
                     paste("Difference in Average Annual State Temperature from ", yr1, " to 1974", sep = ""),
                     paste("Difference in Average Annual State Temperature from 1974 to ", yr1, sep = "")
      ),
      subtitle = 
        "Visualizing how temperatures in the US have changed over the last century relative to temperatures in 1974",
      caption = "Data from NOAA's Global Summary of the Year (GSOY)"
    ) +
    theme(
      plot.title = element_text(size = 24, hjust = .34,), #Original hjust = 0
      plot.subtitle = element_text(size = 16, hjust = .27,), #Tweaked for recording, del hjust
      plot.caption = element_text(size = 14, hjust = 2.5,), #Tweaked for recording, del hjust
      legend.position = "none"
    )
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Year-to-Year Change in US Average Temperature"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(id = "tab",
      tabPanel(title = "Timelapse Graph", value = "Timelapse", 
          sidebarLayout(
              sidebarPanel(
                  sliderInput("year",
                              "Select a year:",
                              min = 1924,
                              max = 2024,
                              value = 1924,
                              ticks = FALSE,
                              animate = animationOptions(interval = 500)
                                          
                  )
              ),
      
              # Show a plot of the generated distribution
              mainPanel(
                 plotOutput("changePlot"),
                 img(src = "legendV2.png", width = "80%", align = "right")
              )
          )  
        ),
        tabPanel(title = "Interactive Graph", value = "Interactive",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year",
                           "Select a year:",
                           min = 1924,
                           max = 2024,
                           value = 1924,
                           ticks = FALSE,
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               h3(textOutput("TitleText"), align = "center"),
               h5("Displays the each state's average annual temperature and the difference
                  in temperatures between the selected year an 1974", align = "center"),
               plotlyOutput("interactivePlot"),
               h5("Data from NOAA's Global Summary of the Year (GSOY)", align = "right"),
               img(src = "legendV2.png", width = "80%", align = "right")
             )    
          )
        )   
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$changePlot <- renderPlot({
      timelapse(input$year)

    })
    
    output$TitleText <- renderText(
      ifelse(input$year < 1975,
             paste("Difference in Average Annual State Temp from", input$year, "to 1974"),
             paste("Difference in Average Annual State Temp from 1974 to", input$year))
    )
    
    output$interactivePlot <- renderPlotly({
      interactive(input$year)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
