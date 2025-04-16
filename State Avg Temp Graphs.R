library(tidyverse)
library(dplyr)
library(data.table)
library(mgsub)
library(usmap)
library(plotly)

#--------------------------Initial Florida Graph-------------------------------#
# Codebook:
#
#TMAX - Maximum temperature
#TAVG - Average Temperature
#TMIN - Minimum temperature


#Read in data set and assign to an object with the name of the state
Florida <- read.csv("Florida.csv")

#Function Definition
state_avg_temp <- function(dat) {
  #Create string with the chosen state's name
  name <- deparse(substitute(dat))
  
  #Restructure the data to give temperature values for each year
  state <- dat |>
    group_by(DATE) |> 
    summarize(
      TMAX = mean(TMAX, na.rm=T),
      TAVG = mean(TAVG, na.rm=T),
      TMIN = mean(TMIN, na.rm=T),
    ) 

  #Graph temperature values
  state |> 
    ggplot(aes(x = DATE, y = TAVG)) +
    geom_point(color = "purple") +
    geom_line(color = "purple") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
    labs(
      title = paste("Annual Average Temperature of ", name, sep = ""),
      x = "Year",
      y = "Temperature (F°)",
      caption = "Data from NOAA's Global Summary of the Year (GSOY)"
    )
}

#Call function with the state object to generate a graph of average temperatures
#from 1900-2025. 
state_avg_temp(Florida)

#-----------------------------Joining State Data-------------------------------#
#
# The code here has been specifically tailored to create the avg_state_temp data set
# If more individual state data needs to be joined, copy this code and make the
#  necessary alterations:
#
#  1) Import one state as a sample and restructure it into its final form
#  2) Initialize an empty data frame (choose the name) using your sample state data
#  3) Create a list of all state data within a SPECIFC folder (change to your folder)
#  4) Alter the restructure function to match the sample's structure
#  5) In mgsub function, change "State Data/" to "your_folder_name/"
#  6) Change info in write_csv function

#Code to format a single dataset
AK <- read_csv("State Data/AK.csv")

AK <- AK |> 
  group_by(DATE) |> 
  summarize(TAVG = mean(TAVG, na.rm=T)) |> 
  filter(DATE >= 1900) |> 
  mutate(
    state = deparse(substitute(AK)), #change substitute when put in function
    temp = ifelse(is.nan(TAVG), NA, TAVG),
    year = paste("temp", DATE, sep = "")
    ) |> 
  select(state, year, temp) |> 
  pivot_wider(names_from = year, values_from = temp)

#Initialize empty data frame using desired structure of initial data set
stateTemp = AK[FALSE,]

#Create list of all data files within the folder
CSVfiles <- list.files(path = "State Data", pattern="\\.csv$", full.names=TRUE)

#Define function for automatically restructuring data into desired format
restructure_data <- function(x, z, filter = 1900) {
  x |> 
    group_by(DATE) |> 
    summarize(TAVG = mean(TAVG, na.rm=T)) |> 
    filter(DATE >= filter) |> 
    mutate(
      state = z,
      temp = ifelse(is.nan(TAVG), NA, TAVG),
      year = paste("temp", DATE, sep = "")
    ) |> 
    select(state, year, temp) |> 
    pivot_wider(names_from = year, values_from = temp)
}

#Import all 50 data sets, restructure, and combine in empty data set
for(x in unique(CSVfiles)){
  y <- read_csv(x)
  z <-  mgsub(x, c("State Data/",".csv"), c("",""))
  y <- restructure_data(y, z)
  stateTemp <<- bind_rows(stateTemp, y)
}

#Save new data set
write.csv(stateTemp, "avg_state_temps.csv")


#-------------------------Average State Temperature Maps-----------------------#

#Read in state temp data
state_temp <- read_csv("avg_state_temps.csv") |> 
  select(-...1)

# Create geometries for US map and map theme
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


#Prepare color schemes for graphs
year_cutpoints <- c(0, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 100)
year_labels <- c("Below 30","30-35", "35-40", "40-45","45-50", "50-55", 
               "55-60", "60-65", "65-70", "70-75", "Above 75")
year_colors <- c("darkblue","#2166ac","#4393c3","#92c5de","#d1e5f0", "#f6e8fc",
               "#fddbc7","#f4a582","#d6604d","#b2182b", "red4")

change_cutpoints <- c(-20, -2.5, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 2.5, 20)
change_labels <- c("More than -2.5","-2.5 to -2", "-2 to -1.5", "-1.5 to -1",
                   "-1 to -.5", "-.5 to 0", "0 to .5", ".5 to 1", "1 to 1.5", 
                   "1.5 to 2", "2 to 2.5", "More than 2.5")
change_colors <- c("darkblue","#2166ac","#4393c3","#92c5de","#d1e5f0", "#eef4f7",
                 "#f7e7e4","#fddbc7","#f4a582","#d6604d","#b2182b", "red4")


#Function to create map for a specific year
year_map <- function(yr) {
  map_data |> 
    select(full, geom, paste("temp", yr, sep = "")) |> 
    rename(temp = paste("temp", yr, sep = "")) |> 
    mutate(range = cut(temp, breaks = year_cutpoints, labels = year_labels)) |> 
    ggplot() +
      geom_sf(aes(fill = range)) +
      scale_fill_manual("Temperature (F°)", values = year_colors, drop = F) +
      my_map_theme() +
      labs(
        title = paste("Average State Temperatures in ", yr, sep = ""),
        caption = "Data from NOAA's Global Summary of the Year (GSOY)"
      ) +
      theme(
        plot.title = element_text(hjust = .8,)
      )
  
}

year_map(2024)

#Function to create change map (specifically for shiny app)
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

change_map(1901)


#Create change map (Old code, continuous color scale)
map_data |> 
  mutate(change = temp2024 - temp2000) |> 
    ggplot() +
      geom_sf(aes(fill = change)) +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", na.value = "grey50", name = "Temperature (F°)"
        ) +
      my_map_theme()+
      labs(
        title = "Change in Average State Temperature from 2000 to 2024",
        caption = "Data from NOAA's Global Summary of the Year (GSOY)"
      ) +
      theme(
        plot.title = element_text(hjust = 0,)
      )

#Attempt to animate graph - Can't get to work!
test <- function(yr1, yr2 = yr1 - 1) {
  plot <- map_data |> 
    select(full, geom, paste("temp", yr1, sep = ""), paste("temp", yr2, sep = "")) |> 
    rename(
      temp1 = paste("temp", yr1, sep = ""),
      temp2 = paste("temp", yr2, sep = ""),
    ) |> 
    mutate(
      temp_diff = temp1 - temp2, 
      difference = cut(temp_diff, breaks = change_cutpoints, labels = change_labels),
      text = paste("<b>", full,"</b>\nAverage temperature in ", yr1, ": ", temp1, "F°", sep = "")
    ) |> 
    ggplot() +
    geom_sf(aes(fill = difference, text = text), color = "black") +
    #scale_fill_manual("Change in Temperature (F°)", values = change_colors, drop = F) +
    scale_fill_gradientn(name = "", colors = change_colors, breaks= change_cutpoints) +
    my_map_theme() +
    labs(
      title = paste("Change in Average State Temperature from ", yr2, " to ", yr1, sep = ""),
      caption = "Data from NOAA's Global Summary of the Year (GSOY)"
    ) +
    theme(
      plot.title = element_text(hjust = 0,)
    )
  
  ggplotly(plot, tooltip = "text") |>
    style(hoveron = "fill")
}

test(1901)
