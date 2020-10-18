# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('vegalite')
library('lubridate')
library('jsonlite')

# Read in .csv data - data from Melbourne Open Data portal
ped_sensor_locations <- as_tibble(read.csv('Data/Pedestrian/Pedestrian_Counting_System_-_Sensor_Locations.csv'))
ped_sensor_locations <- ped_sensor_locations %>% select(sensor_id, sensor_description, latitude, longitude)

# 2020 pedestrian data
ped_data_2020 <- read_csv('Data/Pedestrian/May_2020.csv', trim_ws=FALSE)
ped_data_2020$Date <- as_date(dmy(ped_data_2020$Date))
ped_data_2020 <- gather(ped_data_2020, Location, Count, -Date, -Hour)
ped_data_2020 <- merge(ped_data_2020, ped_sensor_locations, by.x="Location", by.y = "sensor_description")

# 2019 pedestrian data
ped_data_2019 <- read_csv('Data/Pedestrian/May_2019.csv')
ped_data_2019$Date <- as_date(dmy(ped_data_2019$Date))
ped_data_2019 <- gather(ped_data_2019, Location, Count, -Date, -Hour)
ped_data_2019 <- merge(ped_data_2019, ped_sensor_locations, by.x="Location", by.y = "sensor_description")

# Pedestrian data
ped_data <- full_join(ped_data_2020, ped_data_2019)
ped_data$Date <- as.Date(ped_data$Date)
ped_data$Count <- as.integer(ped_data$Count)
ped_data$sensor_id <- as.factor(ped_data$sensor_id)
ped_data <- ped_data %>% mutate(Day = day(Date), Year=as.factor(year(Date)))

minmaxnormalise <- function(x){(x- min(x)) /(max(x)-min(x))}

# Summarised dataset
summ_ped_data <- ped_data %>% filter(Count >= 0)
summ_ped_data <- tibble(summ_ped_data)
summ_ped_data$latitude <- as.character(summ_ped_data$latitude)
summ_ped_data$longitude <- as.character(summ_ped_data$longitude)
summ_ped_data$Hour <- as.integer(summ_ped_data$Hour)
summ_ped_data$Count <- as.numeric(summ_ped_data$Count)
summ_ped_data <- summ_ped_data %>% group_by(Location) %>% mutate(Normalised = minmaxnormalise(Count))
summ_ped_data <- summ_ped_data %>% group_by(Year, Location) %>% mutate(month_mean = mean(Count, na.rm=TRUE), month_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Year, Location, Hour) %>% mutate(hour_mean = mean(Count, na.rm=TRUE), hour_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Year, Location, Day) %>% mutate(day_mean = mean(Count, na.rm=TRUE), day_count=sum(Count, na.rm = TRUE))
summ_ped_data <- summ_ped_data %>% group_by(Location) %>% mutate(location_max = max(Count, na.rm=TRUE), location_min = min(Count, na.rm=TRUE))                                  

top_locations <- summ_ped_data %>% 
  filter(Year=='2019') %>% 
  group_by(Location, Year, month_mean) %>% 
  distinct(Location, Year) %>% 
  ungroup() %>% 
  arrange(desc(month_mean)) %>% 
  top_n(n=30, wt=month_mean) %>% 
  select(Location)

# Filter list to top 30 locations in 2019
summ_ped_data <- summ_ped_data %>% filter(Location %in% top_locations$Location)


serialised <- toJSON(summ_ped_data)
write(serialised, 'data.json')

#vegalite() %>%
  #add_data(summ_ped_data) %>%
  #encode_x("Hour") %>%
  #encode_y("Count")