# Clean Global Environment
rm(list=ls(all=TRUE))

# Load Libraries
library(tidyverse)
library(here)
library(lubridate)

# Load Data
county2020 <- read.csv(here("covid-19-data","us-counties-2020.csv"))
county2021 <- read.csv(here("covid-19-data","us-counties-2021.csv"))
county2022 <- read.csv(here("covid-19-data","us-counties-2022.csv"))
recent <- read.csv(here("covid-19-data","us-counties-recent.csv"))
live <- read.csv(here("covid-19-data","live","us-counties.csv"))

county_total <- rbind(county2020,county2021,county2022,recent,live)

lehigh_case <- county_total %>%
  distinct() %>%
  filter(state == "Pennsylvania") %>%
  filter(county == "Lehigh" ) %>%
  mutate(incr_cases = (cases-lag(cases)))

lehigh_case$incr_cases[1] <- lehigh_case$cases[1]

lehigh_case$date <- as.Date(lehigh_case$date)

ggplot(lehigh_case) +
  geom_line(aes(x=date, y=incr_cases),color="blue")+
  xlab("Date") +
  ylab("New Cases Reported")+
  ggtitle("Lehigh County Covid Cases Tracking")

