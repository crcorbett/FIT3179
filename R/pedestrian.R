# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('lubridate')
library('jsonlite')

# Read in .csv data - data from Melbourne Open Data portal
ped_sensor_locations <- as_tibble(read.csv('data/raw/pedestrian/Pedestrian Counting System - Sensor Locations.csv'))
ped_sensor_locations <- ped_sensor_locations %>% select(sensor_id, latitude, longitude)

# Read in .csv data - data from Melbourne Open Data portal
ped_data <- as_tibble(read.csv('data/raw/pedestrian/Pedestrian Counting System - Dataset.csv')) %>% filter(Year %in% c(2019, 2020)) #%>% filter(Month=='July')
ped_data<- as_tibble(merge(ped_data, ped_sensor_locations, by.x="Sensor_ID", by.y = "sensor_id"))

# Pedestrian data
ped_data$Date_Time <- mdy_hms(ped_data$Date_Time)
ped_data$Sensor_ID <- as.factor(ped_data$Sensor_ID)
ped_data$Day <- as.factor(ped_data$Day)
ped_data$Month <- as.factor(ped_data$Month)
ped_data <- ped_data %>% rename(Count = Hourly_Counts, Location = Sensor_Name, Hour=Time)

minmaxnormalise <- function(x, min, max){(x-min) /(max-min)}

# Summarised dataset
summ_ped_data <- ped_data %>% filter(Count >= 0)
# Calculate 2019 normalisation
min_max <- summ_ped_data %>% filter(Year==2019) %>% group_by(Location) %>% summarise(max_2019 = max(Count), min_2019 = min(Count))
summ_ped_data <- left_join(summ_ped_data, min_max)
summ_ped_data <- summ_ped_data %>% group_by(Location) %>% mutate(Normalised = minmaxnormalise(Count, min_2019, max_2019))

summ_ped_data <- summ_ped_data %>% group_by(Year, Location) %>% mutate(month_mean = mean(Count, na.rm=TRUE), month_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Year, Location, Hour) %>% mutate(hour_mean = mean(Count, na.rm=TRUE), hour_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Year, Location, Day) %>% mutate(day_mean = mean(Count, na.rm=TRUE), day_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Year, Location, Day, Hour) %>% mutate(day_hour_mean = mean(Count, na.rm=TRUE), day_hour_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Location) %>% mutate(location_max = max(Count, na.rm=TRUE), location_min = min(Count, na.rm=TRUE))                                  

top_locations <- summ_ped_data %>% 
  filter(Year==2019) %>% 
  group_by(Location, Year, month_mean) %>% 
  distinct(Location, Year) %>% 
  ungroup() %>% 
  arrange(desc(month_mean)) %>% 
  top_n(n=30, wt=month_mean) %>% 
  select(Location)

# Filter list to top 30 locations in 2019
summ_ped_data <- summ_ped_data %>% filter(Location %in% top_locations$Location) %>% filter(Month=='July')


serialised <- toJSON(summ_ped_data)
write(serialised, 'data/ped_data.json')
