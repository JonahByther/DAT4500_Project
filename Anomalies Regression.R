library(tidyverse)
library(openintro)
library(plotly)
library(dplyr)
library(lubridate)
library(sf)
library(scales)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
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

anom_seatemp <- joined_global_data |>
  ggplot(aes(x = Year)) +
  geom_point(aes(y = Temperature_anomaly)) +
  geom_point(aes(y = Sea_Surface_Temp_Anomaly)) +
  geom_line(aes(y = Temperature_anomaly)) +
  geom_line(aes(y = Sea_Surface_Temp_Anomaly)) +
  ylab("Anomaly Temperature in Celcius") +
  theme_minimal() +
  ggtitle("Sea Surface Anomaly and Temperature Anomaly by Year", 
          subtitle = "Temperature anomaly calculated as difference between a year's average surface temperature \nfrom the 1991-2020 mean. Sea Surface Anomaly calculated as \ndifference between the year and 1971-2000 mean") 

anom_seatemp
  

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

### PCA Analysis Attempt

#Removing all non numerical data and unecessary variables
global_numerical_data <- joined_global_data[-c(1, 3:8, 10:13, 15:17)]

#Normalizing Data
global_normalized_data <- scale(global_numerical_data)


#Applying PCA
pca_global <- princomp(global_normalized_data)
summary(pca_global) 

#Loading matrix of first 2 components
pca_global$loadings[, 1:3]

#Scree Plot
fviz_eig(pca_global, addlabels = TRUE)

#Biplot of Variables
fviz_pca_var(pca_global, col.var = "black", repel = TRUE) +
  xlim(0, 1.2)

#Contribution of each variable to the principal components
fviz_cos2(pca_global, choice = "var", axes = 1:2)

#Combined Biplot and Cos2 Scores (contribution of each variable)
fviz_pca_var(pca_global, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#Regression of PCA
anom_pca_regression <- lm(Temperature_anomaly ~ pca_global$scores[, 1:3], data = joined_global_data)
summary(anom_pca_regression)

#Explanation for 1
  # All variables are increasing at the same rate. Anomaly goes up when Year, Total Rad, Ocean, and Sea surface temp increase. 
  # This is shown where all of component one have same relatively positive loading and coefficient of regression is positive.

#Explanation for component 2
  # Year, Rad, PPM increase. Ocean heat has little to no effect on component 2. Anomaly decreases. 
  # Strong anomaly decrease shows as component increases sea temp anomaly will decrease and vice versa.
  # Higher component 2 means higher rad force and ppm as time goes, but lower sea temp
  # The lagging of sea temp means a overall decrease in temp anomaly





