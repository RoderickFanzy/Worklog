# Started Assignment 2

# Clean the Environment
rm(list=ls(all=TRUE))
getwd()
# Import Datasets
library(here)

states <- read.csv(here("covid-19-data","us-states.csv"))

# Install Packages
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

head(states)

states_pa <- states %>% 
  filter(state == "Pennsylvania" ) %>% 
  mutate(incr_cases = (cases-lag(cases))) %>% 
  mutate(incr_deaths = (deaths-lag(deaths))) %>% 
  replace(is.na(.), 0)

sd(states_pa$incr_cases)
# TAA You failed to calculate special case for row 1     -10
#standard deviation of case increase is 4646.348