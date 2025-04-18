library(tidyverse)
library(openintro)

# Reads csv file
winter_temps_anomalies <- read.csv("winter-temperature-anomalies.csv") 

#Creating Filter for year and average temperature for each month
winter_temps_anomalies_v2 <- winter_temps_anomalies |>
  group_by(Year) |>
  summarize(dec_avg = mean(december),
            jan_avg = mean(january),
            feb_avg = mean(february))

#Restructuring data and renaming columns
winter_temps_anomalies_v2 <- winter_temps_anomalies_v2 |>
  rename(
    December = dec_avg,
    January = jan_avg,
    February = feb_avg
  ) |>
  pivot_longer(cols = c(December, January, February), names_to = "Month", values_to = "average_temp") 
 
# Creating Color fill for bars
winter_temps_anomalies_v2 <- winter_temps_anomalies_v2|>
  mutate(temp_color = ifelse(average_temp < 0, "red", "blue" ))

#Plotting column graph
ggplot(winter_temps_anomalies_v2, aes(x = Year, y = average_temp, fill = temp_color)) +
  geom_col() +
  theme(legend.position = "none") +
  facet_wrap(~Month) +
  ggtitle("Winter Temperature Anomalies, World",
          subtitle = "Calculated as difference between average surface temperature from the mean temperature\n of the same month during the period 1991-2020.") +
  ylab("Average Temperature, measured in Celsius")


