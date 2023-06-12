#Bike-Share Analysis

#Prepare Data
#Install the required packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")
install.packages("readr")

#Load the packages
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(readr)

#Import data
tripdata2022_01 <- read.csv("2022_01_tripdata.csv")
tripdata2022_02 <- read.csv("2022_02_tripdata.csv")
tripdata2022_03 <- read.csv("2022_03_tripdata.csv")
tripdata2022_04 <- read.csv("2022_04_tripdata.csv")
tripdata2022_05 <- read.csv("2022_05_tripdata.csv")
tripdata2022_06 <- read.csv("2022_06_tripdata.csv")
tripdata2022_07 <- read.csv("2022_07_tripdata.csv")
tripdata2022_08 <- read.csv("2022_08_tripdata.csv")
tripdata2022_09 <- read.csv("2022_09_tripdata.csv")
tripdata2022_10 <- read.csv("2022_10_tripdata.csv")
tripdata2022_11 <- read.csv("2022_11_tripdata.csv")
tripdata2022_12 <- read.csv("2022_12_tripdata.csv")


#Process Data
#Look at data
head(tripdata2022_01)
head(tripdata2022_02)
head(tripdata2022_03)
head(tripdata2022_04)
head(tripdata2022_05)
head(tripdata2022_06)
head(tripdata2022_07)
head(tripdata2022_08)
head(tripdata2022_09)
head(tripdata2022_10)
head(tripdata2022_11)
head(tripdata2022_12)

colnames(tripdata2022_01)
colnames(tripdata2022_02)
colnames(tripdata2022_03)
colnames(tripdata2022_04)
colnames(tripdata2022_05)
colnames(tripdata2022_06)
colnames(tripdata2022_07)
colnames(tripdata2022_08)
colnames(tripdata2022_09)
colnames(tripdata2022_10)
colnames(tripdata2022_11)
colnames(tripdata2022_12)

str(tripdata2022_01)
str(tripdata2022_02)
str(tripdata2022_03)
str(tripdata2022_04)
str(tripdata2022_05)
str(tripdata2022_06)
str(tripdata2022_07)
str(tripdata2022_08)
str(tripdata2022_09)
str(tripdata2022_10)
str(tripdata2022_11)
str(tripdata2022_12)

glimpse(tripdata2022_01)
glimpse(tripdata2022_02)
glimpse(tripdata2022_03)
glimpse(tripdata2022_04)
glimpse(tripdata2022_05)
glimpse(tripdata2022_06)
glimpse(tripdata2022_07)
glimpse(tripdata2022_08)
glimpse(tripdata2022_09)
glimpse(tripdata2022_10)
glimpse(tripdata2022_11)
glimpse(tripdata2022_12)

skim_without_charts(tripdata2022_01)
skim_without_charts(tripdata2022_02)
skim_without_charts(tripdata2022_03)
skim_without_charts(tripdata2022_04)
skim_without_charts(tripdata2022_05)
skim_without_charts(tripdata2022_06)
skim_without_charts(tripdata2022_07)
skim_without_charts(tripdata2022_08)
skim_without_charts(tripdata2022_09)
skim_without_charts(tripdata2022_10)
skim_without_charts(tripdata2022_11)
skim_without_charts(tripdata2022_12)

#There are incorrect formats of end_station_id on tripdata2022_06 and tripdata2022_09
#Convert end_station_id format on tripdata2022_06 and tripdata2022_09 to character
tripdata2022_06 <-  mutate(tripdata2022_06, end_station_id = as.character(end_station_id))
tripdata2022_09 <-  mutate(tripdata2022_09, end_station_id = as.character(end_station_id))

#Merge data into one data set
tripdata2022 <- bind_rows(tripdata2022_01,
                          tripdata2022_02,
                          tripdata2022_03,
                          tripdata2022_04,
                          tripdata2022_05,
                          tripdata2022_06,
                          tripdata2022_07,
                          tripdata2022_08,
                          tripdata2022_09,
                          tripdata2022_10,
                          tripdata2022_11,
                          tripdata2022_12)

#Rename member_casual column name to riders_type to avoid the confusion
tripdata2022 <- rename(tripdata2022, riders_type = member_casual)

#Convert day_of_week format to numeric for later calculation
is.factor(tripdata2022$day_of_week)
tripdata2022$day_of_week <- as.numeric(as.character(tripdata2022$day_of_week))
is.numeric(tripdata2022$`day_of_week`)


#Analyze Data
#Descriptive analysis on ride_length (in seconds)
mean(tripdata2022$ride_length)
median(tripdata2022$ride_length)
max(tripdata2022$ride_length)
min(tripdata2022$ride_length)

summary(tripdata2022$ride_length)

#Compare members and casual on riders_type
aggregate(tripdata2022$ride_length ~ tripdata2022$riders_type, FUN = mean)
aggregate(tripdata2022$ride_length ~ tripdata2022$riders_type, FUN = median)
aggregate(tripdata2022$ride_length ~ tripdata2022$riders_type, FUN = max)
aggregate(tripdata2022$ride_length ~ tripdata2022$riders_type, FUN = min)

#Average ride time by each day for members vs casual on riders_type
aggregate(tripdata2022$ride_length ~ tripdata2022$riders_type + tripdata2022$day_of_week, FUN = mean)

#Find most popular start docking-station
tripdata2022 %>% count(start_station_name, sort = TRUE) %>% drop_na()

#Find  most popular end docking-station
tripdata2022 %>% count(end_station_name, sort = TRUE) %>% drop_na()

#Analyze ridership data by riders_type and day_of_week
tripdata2022 %>% 
  group_by(riders_type,day_of_week) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(riders_type, day_of_week)


#Share Data
#Visualization for the number of rides by riders_type
tripdata2022 %>% 
  group_by(riders_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(riders_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = riders_type)) +
  geom_col(position = "dodge")

#Visualization for average duration by riders_type
tripdata2022 %>% 
  group_by(riders_type, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(riders_type, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = riders_type)) +
  geom_col(position = "dodge")

#Type of bike based on riders_type
#Export data set for visualization on Tableau
rideable_type <- tripdata2022 %>% group_by(riders_type) %>% count(rideable_type) %>% drop_na()
write.csv(rideable_type,file="rideable_type.csv",row.names = FALSE)

#Total number of riders per month
#Export data set for visualization on Tableau
count_by_month <- tripdata2022 %>% group_by(month) %>% count(riders_type,month) %>% drop_na()
write.csv(count_by_month,file="count_by_month.csv",row.names = FALSE)

#Total number of riders per day
#Export data set for visualization on Tableau
count_daymonth <- tripdata2022 %>% group_by(day_of_week) %>% count(riders_type,month) %>% drop_na()
write.csv(count_daymonth,file="count_daymonth.csv",row.names = FALSE)