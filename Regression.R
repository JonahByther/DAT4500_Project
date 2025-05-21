library(tidyverse)
library(dplyr)
library(stats)
library(scales)
library(readxl)
library(supernova)
library(apaTables)
library(psych)
library(car)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

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

AGGI <- read.csv("AGGI_Table.csv", skip = 2)

AGGI <- AGGI[-seq(nrow(AGGI),nrow(AGGI)-3),] |> 
  rename(Total_Rad_Force = Total,
         Cumulative_PPM = Total.1,
         Proportion_1990 = `X1990...1`,
         Change_Percent_Previous = `change...`,
         CFC = `CFC.`,
         HFCs = `HFCs.`)
AGGI$Year <- as.numeric(AGGI$Year)

ocean_heat <- read_csv("ocean-heat_fig-1.csv", skip = 6) |> 
  filter(Year >= 1979) |> #1979 chosen for smoother joining
  select(Year, NOAA) |> 
  rename(Ocean_Heat = NOAA)

sea <- read_csv("sea-surface-temp_fig-1.csv", skip = 6) 

sea_surface_temp <- sea |> 
  filter(Year >= 1979) |> #1979 chosen for smoother joining
  select(Year, `Annual anomaly`) |> 
  rename(Sea_Surface_Temp_Anomaly = `Annual anomaly`)

WA_emit <- emissions |> 
  left_join(capita, by = c("Year" = "Year")) |> 
  left_join(WA, by = c("Year" = "Year"))

Combined <- WA_emit |> 
  filter(Year >= 1979) |> 
  left_join(AGGI, by = c("Year" = "Year")) |> 
  left_join(ocean_heat, by = c("Year" = "Year")) |> 
  left_join(sea_surface_temp, by = c("Year" = "Year"))

#Emissions_Oceans <- AGGI |> 
#  left_join(ocean_heat, by = c("Year" = "Year")) |> 
#  left_join(sea_surface_temp, by = c("Year" = "Year"))
#write.csv(Emissions_Oceans, "emissions_oceans.csv")



#--------------------------------Significance Models---------------------------#
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

model <- lm(Temp ~ Sea_Surface_Temp_Anomaly, data = Combined)
summary(model)

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

AGGI_Rad_Model <- lm(Temp ~ Total_Rad_Force, data = Combined)
AGGI_Rad_Model
summary(AGGI_Rad_Model)

AGGI_Cumulative_Model <- lm(Temp ~ Cumulative_PPM, data = Combined)
AGGI_Cumulative_Model
summary(AGGI_Cumulative_Model)

carbonModel <- lm(CO2 ~ Emissions, data = Combined)
carbonModel
summary(carbonModel)

supernova(emitModel)
supernova(capitaModel)
supernova(AGGI_Rad_Model)
supernova(AGGI_Cumulative_Model)



#-------------------------------Multiple regression model----------------------#

Combo_model <- lm(Temp ~ Year + Sea_Surface_Temp_Anomaly + Total_Rad_Force, data = Combined)
summary(Combo_model)
supernova(Combo_model)

std_combo <- lm(scale(Temp) ~ scale(Year) + scale(Sea_Surface_Temp_Anomaly) + scale(Total_Rad_Force), data = Combined)
summary(std_combo)



#--------------------------------------PCA-------------------------------------#

#Variable correlations
Combined |> 
  select(Year, Sea_Surface_Temp_Anomaly, Total_Rad_Force) |> 
  apa.cor.table()

#VIF
vif(Combo_model)

#--PCA--#

#Data normalization
datCombo <- Combined |> 
  select(Year, Sea_Surface_Temp_Anomaly, Total_Rad_Force)
normCombo <- scale(datCombo)


#PCA summary
data.pca <- princomp(normCombo)
summary(data.pca)

data.pca$loadings[, 1:3]

#Scree plot
fviz_eig(data.pca, addlabels = TRUE)

#Biplot of attributes
fviz_pca_var(data.pca, col.var = "black")

#Contribution of variables (Cos2)
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#Biplot + Cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#UPDATED PCA MODEL
pcModel <- lm(Temp ~ data.pca$scores, data = Combined)
summary(pcModel)

#These interpretations come from synthesis of loadings AND model results/coefficients
#Component 1 is when all variables move together in lockstep, temperature just goes up and up; basically average them together
#Component 2 is when Year and Rad force move together, sea surface unusually low. If sea temp dips relative to rad force, downward adjustment
#Component 3 year moves constantly, but rad force doesnt move as much as expected, so theres a negative adjustment (little dip)
#Components 2 and 3 capture variance when one lags (if one var lags behind, temp decreases a bit)
#
#PCA model gives similar results (R2 and F) because it uses the same information as normal model, just rearranges it
#   standardizes all variables (compare thousands of year scale to tenths place of radiative force/sea temp)
#Interesting that all 3 components are significant
#Even though component 3 captures less than 1% of variance, it is significant shows a meaningful trend/relationship
#   If we drop component 3, both R2 and adj R2 have a big decrease, with only a small increase in F
#      via lm(formula = Temp ~ data.pca$scores[, 1:2], data = Combined)
#Year is negative in OG model because temp already rises from Rad and Sea, so year acts as an adjustment (no clear reason why)
#
#
#
#----------------------------------Graphs--------------------------------------#
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

#Cumulative PPM graph
Combined |> 
  ggplot(aes(x= Year, y = Cumulative_PPM)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", color = "red3") +
  theme_classic() +
  scale_x_continuous(expand = c(0,1), breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  labs(
    title = "Annual Atmospheric Concentration of Greenhouse Gases",
    subtitle = "Visualizing how the cumulative concentration of gases has changed over the past 45 years",
    caption = "Measured gases include carbon dioxide, methane, nitrous oxide, CFC, HCFCs, and HFCs.
    (CFC, HCFCs, and HFCs are synthetic gases, commonly used as aerosols or refrigerants)",
    y = "Carbon Dioxide-Equivalent PPM"
  )

#Radiative forcing graph
Combined |> 
  ggplot(aes(x= Year, y = Total_Rad_Force)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", color = "red3") +
  theme_classic() +
  scale_x_continuous(expand = c(0,1), breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = .5)) +
  labs(
    title = "Annual Levels of Global Radiative Forcing",
    subtitle = "Visualizing how greenhouse gases increase the amount of energy in Earth's atmosphere",
    caption = "Radiative forcing measures the difference between energy (in the form of radiation) entering the atmosphere
    and leaving the atmosphere. Positive values indicate that there is more energy entering Earth than leaving
    Earth, which results in warming",
    y = "Global Radiative Forcing (W/m^2)"
  )

#Sea temp graph
sea |> 
  ggplot(aes(x= Year, y = `Annual anomaly`)) +
  geom_line(aes(y=0), linetype = 2, linewidth = 1, color = "pink4") +
  geom_text(x = 1910, y = .07, label = "1971-2000 Average Temperature", color = "pink4") +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", color = "red3") +
  theme_classic() +
  scale_x_continuous(expand = c(0,1), breaks= seq(1880, 2020, by = 10)) +
  labs(
    title = "Annual Sea Surface Temperature Anomalies",
    subtitle = "Visualizes how the average surface temperature of the world's oceans has\nchanged since 1880",
    caption = "Temperature anomalies describe how much a given year's temperature
    deviates from the long-term average temperature of the specified period.",
    y = "Temperature Anomaly (°F)"
  )

#Visual representation of multiple regression model - adjustments made to 640:480 resolution
Combined |> 
  ggplot(aes(x = Year, y = Temp)) +
  geom_line(linewidth = .5) +
  geom_smooth(method = "lm", color = "orange") +
  geom_point(aes(color = Sea_Surface_Temp_Anomaly, size = Total_Rad_Force)) +
  geom_point(aes(size = Total_Rad_Force), shape = 1, stroke = .85) +
  scale_color_gradientn(colors = c("yellow", 'red', "red4"), name = "Sea Surface\nTemperature\nAnomaly (°F)") +
  scale_size(range = c(1,5), name = "Global Radiative\nForcing (W/m^2)") +
  labs(
    title = "Predictors of Washington's Average Annual Temperature",
    subtitle =
      "Visualizing the impacts of sea surface temperature and global radiative forcing on Washington's
temperature over time",
    caption = 
    "Sea Surface Temperature Anomaly is calculated by comparing each year's global average sea surface temperature \n     to the average surface temperature from 1971-2000.\nRadiative forcing measures the difference between energy (in the form of radiation) entering the atmosphere and leaving\n     the atmosphere. Positive values indicate that there is more energy entering Earth, resulting in warming.",
    y = "Temperature (°F)"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
  