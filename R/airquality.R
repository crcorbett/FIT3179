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
  filter(Month == 7)


ggplot(airquality_data) +
  geom_boxplot(aes(x=Date, y=median, colour=Type)) +
  facet_wrap(~Type, scales="free")

ggplot(airquality_data) +
    geom_tile(aes(x=Date, y=Type, fill=median)) +
    facet_wrap(~year(Date), scales = "free")
