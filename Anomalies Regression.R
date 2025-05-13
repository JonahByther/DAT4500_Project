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

# Regression Summary

seasurface_anom_sig <- lm(Sea_Surface_Temp_Anomaly ~ Temperature.anomaly, data = joined_data)
summary(ocean_anom_sig)

ghgs_anom_sig <- lm(Annual_PPM ~ Temperature.anomaly, data = joined_data)
summary(ghgs_anom_sig)

ghgs_ocean_sig <- lm(Ocean_Heat ~ Annual_PPM, data = joined_data)
summary(ghgs_ocean_sig)

#Multi regression for different GHGs

ghg_multi_sig <- lm(Temperature.anomaly ~ Cumulative_PPM + Ocean_Heat, data = joined_data)
summary(ghg_multi_sig)
# Visuals


joined_global_data |>
  ggplot(aes(x = Temperature.anomaly, y = Sea_Surface_Temp_Anomaly)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Temperature Anomaly vs. Sea Surface Temperature Anomaly",
       subtitle = "Temperature Anomaly calculated as difference between average surface temperature from\nthe mean temperature of the same month during the period 1991-2020. \nSea Surface Anomaly uses 1971 - 2000 average to depict change.") +
  ylab("Sea Surface Temperature Anomaly") +
  xlab("Temperature Anomaly")

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
