library(tidyverse)

# Codebook:
#
#TMAX - Maximum temperature
#TAVG - Average Temperature.
#TMIN - Minimum temperature
#


#Read in data set and assign to an object with the name of the state
Florida <- read.csv("Florida.csv")


#Call function with the state object to generate a graph of average temperatures
#from 1900-2025. 
state_avg_temp(Florida)

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