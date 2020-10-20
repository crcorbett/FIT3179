# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('vegalite')
library('lubridate')
library('jsonlite')

# Read in Air Quality data
airquality_data_2019 <- as_tibble(read_csv('Data/Air Quality/AQI-2019.csv')) %>% filter(City=='Melbourne')
airquality_data_2020 <- as_tibble(read_csv('Data/Air Quality/AQI-2020.csv')) %>% filter(City=='Melbourne')
 
airquality_data <- full_join(airquality_data_2019, airquality_data_2020)

airquality_data <- airquality_data %>% mutate(Day=day(Date), Month=month(Date))

# Filter to July
airquality_data_june <- airquality_data %>% filter(Month==6)
