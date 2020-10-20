# Title     : TODO
# Objective : TODO
# Created by: cooper
# Created on: 9/10/20

library('tidyverse')
library('vegalite')
library('lubridate')
library('jsonlite')

# Read in .csv data - data from Melbourne Open Data portal
full_ped_data <- as_tibble(read.csv('Data/Pedestrian/pedestrian_data.csv')) %>% filter(Year %in% c(2019, 2020)) %>% filter(Month=='July')
