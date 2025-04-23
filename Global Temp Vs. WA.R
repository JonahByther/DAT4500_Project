library(tidyverse)
library(openintro)
library(lubridate)

January_global_temp <- read.csv("January_global_temp.csv", comment.char="#")
WA_county <- read.csv("WA_county.csv")

#Filtering just king county and applying same base year difference
king_county <- WA_county |>
  filter(COUNTY == "King County") |>
  mutate(
    Year = year(DATE),
    Month = month(DATE)
  ) |>
  mutate(
    baseline_mean = mean(TAVG[ Year >= 1901 & Year <= 2000], na.rm = TRUE),
    Anomaly = TAVG - baseline_mean
  )

#Filter for January
january_king <- king_county |>
  filter(Month == 1)

#Conversion to F

January_global_temp <- January_global_temp |>
  group_by(Year) |>
  mutate(Anomaly = Anomaly * 1.8)

#Adding Source Identifier
january_king <- january_king |>
  mutate(Source = "King County")

January_global_temp <- January_global_temp |>
  mutate(Source = "Global")

jan_combined_anomalies <- bind_rows(january_king, January_global_temp)


jan_combined_anomalies |>
  ggplot(aes(x = Year, y = Anomaly, color = Source)) + 
  geom_line(method = "lm") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Anomaly Temperature Comparison \nGlobal vs. King County in January",
       subtitle = "Anomaly calculated as difference between average temperature \nand baseline mean (1990-2000)") +
  ylab("Anomaly, measured in Farenheit") +
  theme_minimal()

