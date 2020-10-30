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

minmaxnormalise <- function(x, min, max){(x-min) /(max-min)}

airquality_data <- airquality_data %>%
  select(-Country, -City) %>%
  mutate(Day=day(Date), Month=month(Date)) %>%
  rename(Type = Specie)
airquality_data$Type <- as_factor(airquality_data$Type)

# Filter to certain environmental metrics
airquality_data <- airquality_data %>%
  filter(Type %in% c("so2")) %>%
  mutate(day = day(Date), month = month(Date), year = year(Date), wday = weekdays(Date)) %>%
  mutate(wday = fct_reorder(wday, wday(Date, week_start = getOption(" lubridate.week.start", 1)))) %>%
  mutate(day_type = case_when(
    wday == 'Monday' ~ 'Weekday',
    wday == 'Tuesday' ~ 'Weekday',
    wday == 'Wednesday' ~ 'Weekday',
    wday == 'Thursday' ~ 'Weekday',
    wday == 'Friday' ~ 'Weekday',
    wday == 'Saturday' ~ 'Weekend',
    wday == 'Sunday' ~ 'Weekend')
  )

airquality_data <- airquality_data %>%
  group_by(Type) %>%
  mutate(normalise = minmaxnormalise(median, min(median), max(median))) %>%
  ungroup()

serialised <- toJSON(airquality_data)
write(serialised, 'data/airquality_epa.json')

ggplot(airquality_data) +
  geom_boxplot(aes(x=Date, y=median, colour=Type)) +
  facet_wrap(~Type, scales="free")

ggplot(airquality_data) +
  geom_tile(aes(x=wday, y=factor(month(Date, label = T)), fill=normalise, width=0.9, height=0.9), na.rm = T) +
  facet_grid(week(Date)~year(Date), scales = "free") +
  scale_y_discrete() +
  theme_bw() +
  scale_fill_gradient2(high = "red")


#
#
#

airquality <- as_tibble(read_csv("data/raw/airquality/Melbourne - Microclimate Sensor Readings.csv"))
airquality$local_time <- ymd_hms(airquality$local_time)
airquality$type <- as_factor(airquality$type)
airquality <- airquality

airquality <- airquality %>% 
  group_by(site_id, type, date=date(local_time)) %>% 
  mutate(mean=mean(value)) %>% 
  filter(site_id=="arc1050")

minmaxnormalise <- function(x, min, max){(x-min) /(max-min)}

airquality %>% 
  mutate(normalised=rnorm(mean, mean = 0, sd = 1)) %>% 
  ggplot() +
  geom_tile(aes(x=date, y=type, fill=mean, group=type))

# serialised <- toJSON(airquality_data)
# write(serialised, 'data/airquality_epa.json')
