# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('vegalite')
library('lubridate')
library('jsonlite')

# Read in Apple Mobility data
mobility_data <- as_tibble(read_csv('data/raw/mobility/applemobilitytrends-2020-10-05.csv'))

# Filter to Melbourne results
melbourne_mobility_data <- mobility_data %>% filter(region=="Melbourne")

# 
melbourne_mobility_data <- melbourne_mobility_data %>% select(-geo_type, -region, -alternative_name, -'sub-region', -country)
melbourne_mobility_data <- melbourne_mobility_data %>% gather(Date, Value, -transportation_type)
melbourne_mobility_data$Date <- ymd(melbourne_mobility_data$Date)
melbourne_mobility_data <- melbourne_mobility_data %>% mutate(Day=day(Date), Month=month(Date))

serialised <- toJSON(melbourne_mobility_data)
write(serialised, 'data/melbourne_mobility_data.json')

# Filter results
melbourne_mobility_data_filtered <- melbourne_mobility_data %>% filter(Month==7)

# Plot 
ggplot(melbourne_mobility_data, mapping = aes(x=Date, y=Value)) + 
  geom_line(aes(x=Date, y=100)) +
  geom_smooth(aes(color=transportation_type, fill="red"))

# Plot  
ggplot(melbourne_mobility_data_filtered) + 
  geom_line(aes(x=Date, y=Value, color=transportation_type)) +
  geom_line(aes(x=Date, y=100))

