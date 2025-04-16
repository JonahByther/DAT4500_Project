library(tidyverse)
library(openintro)
library(plotly)

# Reads csv file
temp_anomalies <- read.csv("country-level-monthly-temperature-anomalies.csv")

# Rounding to 2 decimal places to replicate DF
temp_anomalies_v2 <- temp_anomalies |>
  mutate(across(January:December, ~ round(.x, 2)))


#Creating Filter for year and average temperature for each month
temp_anomalies_v2 <- temp_anomalies |>
  group_by(Year) |>
  summarize(january = mean(january),
            february = mean(february),
            march = mean(march),
            april = mean(april),
            may = mean(may),
            june = mean(june),
            july = mean(july),
            august = mean(august),
            september = mean(september),
            october = mean(october),
            november = mean(november),
            december = mean(december)
  )

#Restructuring data
temp_anomalies_v2 <- temp_anomalies_v2 |>
  pivot_longer(cols = c(January:December), names_to = "Month", values_to = "anomaly_temp") 

#Baseline year to find anomalies value
baseline <- temp_anomalies |>
  filter(Year >= 1991 & Year <= 2020) |>
  summarize(january = sum(january),
            february = mean(february),
            march = mean(march),
            april = mean(april),
            may = mean(may),
            june = mean(june),
            july = mean(july),
            august = mean(august),
            september = mean(september),
            october = mean(october),
            november = mean(november),
            december = mean(december)) |>
  pivot_longer(cols = c(january:december), names_to = "Month", values_to = "baseline_temp")

#Joining baseline with winter temp
winter_temps_anomalies_v2 <- winter_temps_anomalies_v2 |>
  left_join(baseline, by = "Month") |>
  mutate(anomaly = average_temp - baseline_temp)
 
# Creating Color fill for bars
temp_anomalies_v2 <- temp_anomalies_v2 |>
  mutate(temp_color = ifelse(average_temp < 0, "red", "blue" ))

#Plotting column graph
season_plot <- ggplot(temp_anomalies_v2, aes(x = Year, y = anomaly_temp, fill = temp_color)) +
  geom_col() +
  theme(legend.position = "none") +
  facet_wrap(~Month) +
  ggtitle("Temperature anomalies by month, World",
          subtitle = "Calculated as difference between average surface temperature from the mean temperature\n of the same month during the period 1991-2020.") +
  ylab("Average Temperature, measured in Celsius")

#Creating Seasons to input in filter
winter_months <- c("December", "January", "February")
spring_months <- c("March", "April", "May")
summer_months <- c("June", "July", "August")
fall_months <- c("September", "October", "November")

#Interactivity in plot
p <- temp_anomalies_v2 |>
  mutate(text = paste("Year:", Year, "</b>",
                      "<br>World:", anomaly_temp, "(\u00B0C)")) |>
  filter(Month %in% winter_months) |>
  ggplot(aes(x = Year, y = anomaly_temp, fill = temp_color)) +
  geom_col(aes(text = text)) +
  theme(legend.position = "none") +
  facet_wrap(~Month) +
  labs(title = "Temperature anomalies by month, World \n Calculated as difference between average surface temperature from the mean temperature of the same month during the period 1991-2020.") +
  ylab("Average Temperature, measured in Celsius")

ggplotly(p, tooltip = "text")

# Displays max temps by month and year
temp_anomalies_v2 |>
  group_by(Month) |>
  filter(anomaly_temp == max(anomaly_temp, na.rm = TRUE)) |>
  select(Month, anomaly_temp, Year) |>
  arrange(desc(anomaly_temp))
