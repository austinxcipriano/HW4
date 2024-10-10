#In-Class Prompts
install.packages(c("dplyr", "lubridate", "tidyverse"))
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

weather <- read.csv("/cloud/project/campus_weather.csv", na.strings = "#N/A")
weather$DateF <- mdy_hm(weather$Date)

MetaData <- read.csv("/cloud/project/meter_weather_metadata.csv", na.strings = "#N/A")

SensorLog <- read.csv("/cloud/project/Sensor log.csv", na.strings = "#N/A")

interval <- weather$DateF[-length(weather$DateF)] %--% weather$DateF[-1]

# Set up time intervals in a vector of dates
timeInterval <- function(x){
  x[-length(x)] %--% x[-1] 
}

timeInterval(weather$DateF)

for(i in 1:6){
  print(paste("example", i))
}

seqEx <- c(1,4,6)
for (i in seqEx){
  print(paste("example", i))
}

chEx <- character()
for(i in 1:6){
  chEx[i] <- paste("example", i)
}

numEx <- numeric()
for(i in 1:6){
  numEx[i] <- 6*i
}

# In-Class Prompt 1
# Calculate a rolling average of air temperatures over eight 15 min measurements 
# (2 hours) for January of 2022 using a for loop. Make a plot of the 15 minute 
# air temperature and the rolling average.

# create a month column
weather$month <- month(weather$DateF)

# create a year column
weather$year <- year(weather$DateF)

Jan22 <- weather %>%
  filter(month == 1 & year == 2022)

mean(Jan22$AirTemp[1:8])

rollAvgTemp <- numeric()
for(i in 8:nrow(Jan22)){
  rollAvgTemp[i] <- mean(Jan22$AirTemp[(i-7):i])
}
Jan22$rollAvgTemp <- rollAvgTemp

ggplot(data = Jan22, aes(x = DateF, y = rollAvgTemp)) +
  geom_line(na.rm = T) +
  labs(x = "Date", y = "Rolling Average Temperature")
  
#In-Class Prompt 2
#You want to see if the solar radiation measurements experienced any issues with 
#build up or accumulation on the sensor in May and June of 2021. Make an 
#assessment with your group.
  
MayJun21 <- weather %>%
    filter(month %in% c(5, 6) & year == 2021)
  
ggplot(data = MayJun21, aes(x = DateF, y = SolRad)) +
    geom_line() +
    labs(x = "Date", y = "Solar Radiation")
  
#Homework Prompt 1
#As the weather station data manager, you been asked to share precipitation data 
#with the village of Clinton. You want to ensure that there are no issues with 
#the bird excrement or frozen precipitation. You want to exclude any precipitation 
#that occurs when the air temperature is below zero. You also want to check that 
#no precipitation measurements are used if the X and Y level observations are 
#more than 2 degrees.

weather$Precipitation <- ifelse((abs(weather$XLevel) > 2 | abs(weather$YLevel) > 2 | weather$AirTemp < 0),
                                 NA, weather$Precip)

#Indicate how many missing precipitation values are in your data. Do you think 
#there might be any additional issues with the precipitation data to consider 
#before sending the data to the village. Generally describe (do not code) what 
#you might need to do for further data cleaning.

sum(as.integer(is.na(weather$Precipitation)))

#Homework Prompt 2
#Create a data flag that warns a user if the battery voltage falls below 8.5 
#Volts. Explain how you set up the flag.

weather$LowVoltageFlag <- ifelse(weather$BatVolt <= 8.5, 1, 0)

#Homework Prompt 3
#You should also create a function that checks for observations that are in 
#unrealistic data ranges in air temperature and solar radiation. Explain how 
#your function works.

OutlierFunction <- function(x){
  mean_x <- mean(x, na.rm = T)
  sd_x <- sd(x, na.rm = T)
  z_score <- (x - mean_x) / sd_x
  Outliers <- ifelse(abs(z_score) > 3, 1, 0)
}

weather$AirTempOutliers = OutlierFunction(weather$AirTemp)
weather$SolarRadiationOutliers = OutlierFunction(weather$SolRad)

#Homework Prompt 4
#Make a plot of winter air temperatures in Jan - Mar of 2021. Check for 
#persistence issues that might indicate snow accumulation on the sensor. 
#Describe whether you think this might be an issue.

JanMar21 <- weather %>%
  filter(month %in% seq(1, 3) & year == 2021)

ggplot(data = JanMar21, aes(x = DateF, y = AirTemp)) +
  geom_line() +
  labs(x = "Date", y = "Air Temperature")

#Homework Prompt 5
#You are asked for total daily precipitation in March and April of 2021. Use a 
#for loop to exclude (convert to NA) any days that include temperatures less 
#than 35 degrees F on that day or the day prior to ensure that measurements are 
#not likely to be affected by snow accumulation on the sensor. How many daily 
#observations have precipitation observations (not a NA) in your final data table?

MarApr <- weather %>% filter(year == 2021 & month %in% c(3,4))
MarApr <- MarApr %>%
  mutate(Fahrenheit = (AirTemp*(9/5)) + 32)

for (i in (2: length(MarApr$AirTemp))) {
  MarApr$Precip[i] <- ifelse(MarApr$Fahrenheit[i] < 35 | MarApr$Fahrenheit[i-1] < 35, 
    NA, MarApr$Precip[i])
}

sum(as.integer(!is.na(MarApr$Precip)))
