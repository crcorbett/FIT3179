# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('lubridate')
library('jsonlite')

# Read in Air Quality data
airquality_data_2019 <- as_tibble(read_csv('data/raw/airquality/AQI-2019.csv')) %>% filter(City=='Melbourne')
airquality_data_2020 <- as_tibble(read_csv('data/raw/airquality/AQI-2020.csv')) %>% filter(City=='Melbourne')

airquality_data <- full_join(airquality_data_2019, airquality_data_2020)

airquality_data <- airquality_data %>%
  select(-Country, -City) %>%
  mutate(Day=day(Date), Month=month(Date)) %>%
  rename(Type = Specie)
airquality_data$Type <- as_factor(airquality_data$Type)

# Filter to certain environmental metrics
airquality_data <- airquality_data %>%
  filter(Type %in% c("co", "pm10", "no2", "o3", "so2","pm25")) %>%
  filter(Month %in% c(6, 7)) %>%
  filter(Type=="no2") %>%
  mutate(day=day(Date), month=month(Date), year=year(Date)) %>%
  mutate(day_month=paste(day, month, sep="/"))


ggplot(airquality_data) +
  geom_boxplot(aes(x=Date, y=median, colour=Type)) +
  facet_wrap(~Type, scales="free")

ggplot(airquality_data) +
    geom_tile(aes(x=factor(month(Date)), y=factor(year(Date)), fill=median, alpha=median))

#
#
#

airquality <- as_tibble(read_csv("data/raw/airquality/Melbourne - Microclimate Sensor Readings.csv"))
airquality$local_time <- ymd_hms(airquality$local_time)
airquality$type <- as_factor(airquality$type)
airquality <- airquality %>%
  filter(type %in% c("PM2.5-EPA-1h-NOPK-EPA-24h", "PM10-EPA-1h-NOPK-EPA-24h"))

airquality <- airquality %>% 
  group_by(site_id, type, date=date(local_time)) %>% 
  mutate(mean=mean(value)) %>% 
  filter(site_id=="arc1050")

minmaxnormalise <- function(x, min, max){(x-min) /(max-min)}

airquality %>% 
  mutate(normalised=rnorm(mean, mean = 0, sd = 1)) %>% 
  ggplot() +
  geom_tile(aes(x=date, y=type, fill=mean, group=type))
