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

change_cutpoints <- c(-20, -2.5, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 2.5, 20)
change_labels <- c("More than -2.5","-2.5 to -2", "-2 to -1.5", "-1.5 to -1",
                   "-1 to -.5", "-.5 to 0", "0 to .5", ".5 to 1", "1 to 1.5", 
                   "1.5 to 2", "2 to 2.5", "More than 2.5")
change_colors <- c("darkblue","#2166ac","#4393c3","#92c5de","#d1e5f0", "#eef4f7",
                   "#f7e7e4","#fddbc7","#f4a582","#d6604d","#b2182b", "red4")

change_map <- function(yr1, yr2 = yr1 - 1) {
  map_data |> 
    select(full, geom, paste("temp", yr1, sep = ""), paste("temp", yr2, sep = "")) |> 
    rename(
      temp1 = paste("temp", yr1, sep = ""),
      temp2 = paste("temp", yr2, sep = ""),
    ) |> 
    mutate(
      temp_diff = temp1 - temp2, 
      difference = cut(temp_diff, breaks = change_cutpoints, labels = change_labels)
    ) |> 
    ggplot() +
    geom_sf(aes(fill = difference)) +
    scale_fill_manual("Change in Temperature (F°)", values = change_colors, drop = F) +
    my_map_theme() +
    labs(
      title = paste("Change in Average State Temperature from ", yr2, " to ", yr1, sep = ""),
      caption = "Data from NOAA's Global Summary of the Year (GSOY)"
    ) +
    theme(
      plot.title = element_text(hjust = 0,)
    )
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
                        min = 1924,
                        max = 2024,
                        value = 1924,
                        ticks = FALSE,
                        animate = animationOptions(interval = 500)
                                    
            )
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
      change_map(input$year)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
