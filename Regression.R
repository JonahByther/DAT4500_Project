library(tidyverse)
library(dplyr)
library(stats)
library(scales)
library(readxl)
library(supernova)
library(lm.beta)

WA <- read.csv("WAmonth.csv", skip = 3) |> 
  separate(col = Date, into = c("Year", "Month"), sep = 4)|> 
  group_by(Year) |> 
  summarize (Temp = mean(Value)) |> 
  ungroup()
WA$Year <- as.numeric(WA$Year)

emissions <- read_excel("US Energy CO2 Emissions.xlsx", skip = 4) |> 
  filter(State == "Washington") |> 
  select(`1970`:`2022`) |> 
  pivot_longer(cols = `1970`:`2022`, names_to = "Year", values_to = "Emissions")
emissions$Year <- as.numeric(emissions$Year)

capita <- read_excel("energy_CO2_per_capita.xlsx", skip = 4) |> 
  filter(State == "Washington") |> 
  select(`1970`:`2022`) |> 
  pivot_longer(cols = `1970`:`2022`, names_to = "Year", values_to = "Per_Capita")
capita$Year <- as.numeric(capita$Year)

WA_emit <- emissions |> 
  left_join(capita, by = c("Year" = "Year")) |> 
  left_join(WA, by = c("Year" = "Year"))

AGGI <- read.csv("AGGI_Table.csv", skip = 2)

AGGI <- AGGI[-seq(nrow(AGGI),nrow(AGGI)-3),] |> 
  rename(Annual_PPM = Total,
         Cumulative_PPM = Total.1,
         Proportion_1990 = `X1990...1`,
         Change_Percent_Previous = `change...`,
         CFC = `CFC.`,
         HFCs = `HFCs.`)
AGGI$Year <- as.numeric(AGGI$Year)

AGGI_emit <- WA_emit |> 
  filter(Year >= 1979) |> 
  left_join(AGGI, by = c("Year" = "Year"))

#Significance Models
emitSig <- lm(Emissions ~ Year, data = WA_emit)
emitSig
summary(emitSig)

stdEmit <- lm(scale(Emissions) ~ scale(Year), data = WA_emit)
summary(stdEmit)

capitaSig <- lm(Per_Capita ~ Year, data = WA_emit)
capitaSig
summary(capitaSig)

stdCap <- lm(scale(Per_Capita) ~ scale(Year), data = WA_emit)
summary(stdCap)

tempSig <-lm(Temp ~ Year, data = WA_emit)
tempSig
summary(tempSig)

stdTemp <-lm(scale(Temp) ~ scale(Year), data = WA_emit)
summary(stdTemp)

#Predictive Models
emitModel <- lm(Temp ~ Emissions, data = WA_emit)
emitModel
summary(emitModel)
supernova(emitModel)

stdEM  <- lm(scale(Temp) ~ (Emissions), data = WA_emit)
summary(stdEM)

capitaModel <- lm(Temp ~ Per_Capita, data = WA_emit)
capitaModel
summary(capitaModel)

AGGI_Annual_Model <- lm(Temp ~ Annual_PPM, data = AGGI_emit)
AGGI_Annual_Model
summary(AGGI_Annual_Model)

AGGI_Cumulative_Model <- lm(Temp ~ Cumulative_PPM, data = AGGI_emit)
AGGI_Cumulative_Model
summary(AGGI_Cumulative_Model)

carbonModel <- lm(CO2 ~ Emissions, data = AGGI_emit)
carbonModel
summary(carbonModel)

supernova(emitModel)
supernova(capitaModel)
supernova(AGGI_Annual_Model)
supernova(AGGI_Cumulative_Model)


#Graphs
WA_emit |> 
  ggplot(aes(x= Year, y = Emissions)) +
  geom_line(color = "black") +
  geom_point(aes(color = Temp)) +
  geom_smooth(method = "lm", color = "black", alpha = .3, linewidth = .6) +
  theme_classic() +
  scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_color_gradientn(colors = c("yellow", 'red', "red4"), name = "Temperature (°F)") +
  labs(
    title = "Washington State's Annual Energy-Related Carbon Emissions",
    subtitle = "Measures how much CO2 is emitted when fossil fuels are burned to produce electricity",
    y = "Metric Tons of CO2 (Millions)"
  )

WA_emit |> 
  ggplot(aes(x= Year, y = Per_Capita)) +
  geom_line(color = "black") +
  geom_point(aes(color = Temp)) +
  geom_smooth(method = "lm", color = "black", alpha = .3, linewidth = .6) +
  theme_classic() +
  scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_color_gradientn(colors = c("yellow", 'red', "red4"), name = "Temperature (°F)") +
  labs(
    title = "Washington State's Annual Energy-Related CO2 Emissions Per Capita",
    subtitle = "Measures how much CO2 is emitted when fossil fuels are burned to produce electricity",
    y = "Metric Tons of CO2 Per Person"
  )

WA |> 
  ggplot(aes(x= Year, y = Temp)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    title = "Washington State's Annual Average Temperature",
    y = "Temperature (°F)"
  )

AGGI_emit |> 
  ggplot(aes(x= Year, y = Cumulative_PPM)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  theme_classic() +
  scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    title = "Cumulative PPM",
    y = "Greenhouse Gas PPM"
  )
