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

serialised <- toJSON(restrictions)
write(serialised, 'data/restrictions.json')
