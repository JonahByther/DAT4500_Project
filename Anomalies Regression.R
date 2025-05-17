library(tidyverse)
library(openintro)
library(plotly)
library(dplyr)
library(lubridate)
library(sf)
library(scales)
annual_anomalies <- read.csv("annual-temperature-anomalies.csv")
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

ggplotly(anom_ocean_heat, tooltip = "text")



joined_global_data |>
  ggplot(aes(x = Temperature.anomaly, y = Annual_PPM)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Temperature Anomaly and Annual GHG Concentrations in PPM") +
  xlab("Temperature Anomaly") +
  ylab("Annual Greenhouse Gas Concentrations in PPM")

joined_global_data |>
  ggplot(aes(x = Annual_PPM, y = Ocean_Heat)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Annual Greenhouse Gas Concentrations and Ocean Heat") +
  xlab("Annual PPM") +
  ylab("Ocean Heat Change from Previous Year")

# Regression Summary

seasurface_anom_sig <- lm(Temperature_anomaly~ Ocean_Heat, data = joined_global_data)
summary(ocean_anom_sig)

ghgs_anom_sig <- lm(Total_Rad_Force ~ Temperature.anomaly, data = joined_global_data)
summary(ghgs_anom_sig)

ghgs_ocean_sig <- lm(Ocean_Heat ~ Annual_PPM, data = joined_global_data)
summary(ghgs_ocean_sig)

#Multi regression for different GHGs

ghg_multi_sig <- lm(Temperature_anomaly ~ Year + Ocean_Heat + Total_Rad_Force, data = joined_global_data)
summary(ghg_multi_sig)
