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

restrictions_date <- tibble(
  Measure = c("Stage One", "Stage Two", "Stage Three", "Stage Two", "Stage Three", "Stage Four", "First Step", "Second Step", "Second Step (Extended"), 
  Start=c("2020-03-23", "2020-03-26", "2020-03-31", "2020-06-01", "2020-07-08", "2020-08-02", "2020-09-14", "2020-09-28", "2020-10-19"), 
  End=c("2020-03-26", "2020-03-31", "2020-06-01", "2020-07-08", "2020-08-02", "2020-09-14", "2020-09-28", "2020-10-19", "2020-10-26")
)
restrictions_date$Start <- ymd(restrictions_date$Start)
restrictions_date$End <- ymd(restrictions_date$End)

restrictions_date <- restrictions_date %>% mutate(Middle=Start + ((End-Start)/2))

serialised <- toJSON(restrictions_date)
write(serialised, 'data/restrictions_date.json')
