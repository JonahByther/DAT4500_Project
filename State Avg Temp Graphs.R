library(tidyverse)
library(dplyr)
library(data.table)
library(mgsub)

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



