# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('lubridate')
library('jsonlite')

restrictions <- tibble(read_csv("data/raw/restrictions/Oxford Policy Tracker - Dataset.csv")) %>%
  filter(CountryName == "Australia")
restrictions$Date <- ymd(restrictions$Date)
restrictions <- restrictions %>%
  select(Date, ConfirmedCases, ConfirmedDeaths, GovernmentResponseIndex)

coronavirus <- tibble(read_csv("data/raw/coronavirus/COVID_AU_state_daily_change.csv")) %>%
  filter(state=="Victoria") %>%
  rename(Date = date)

restrictions <- right_join(restrictions, coronavirus)

ggplot(restrictions) +
  geom_line(aes(x=Date, y=GovernmentResponseIndex/80*700)) +
  geom_line(aes(x=Date, y=confirmed)) +
  scale_y_continuous(name = "Daily Coronavirus cases", sec.axis = sec_axis(~./700*80 ,name="Government Response Index"))

serialised <- toJSON(restrictions)
write(serialised, 'data/restrictions.json')

restrictions_date <- tibble(data.frame(Date=c('2020-03-16', '2020-07-19', '2020-07-09', '2020-08-02'), Measure=c("State of Emergency",'Face Masks', 'Stage 3', 'Stage 4')))
restrictions_date$Date <- ymd(restrictions_date$Date)

serialised <- toJSON(restrictions_date)
write(serialised, 'data/restrictions_date.json')