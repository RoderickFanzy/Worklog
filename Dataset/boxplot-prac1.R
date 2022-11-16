#Clean Global Environment
rm(list = ls(all=T))

#Import required packages
library(tidyverse)
library(here)
library(readr)
library(gridExtra)

#Import dataset
tank <- read_csv(here("Dataset","WOT.csv"))
summary(tank)

tank$WR <- gsub('%','',tank$WR)
tank$WR <- as.numeric(tank$WR)
tank <- tank[tank$Battles > 3,]

g1 <- ggplot()+
  geom_boxplot(data = tank, aes(x=reorder(Nation,WR), y=WR))+
  labs(x = "Nation Played",
       y = "Win Rate (WR)",
       title = "Stats by Nation Played")
g2 <- ggplot()+
  geom_boxplot(data = tank, aes(x=reorder(Class,WR), y=WR))+
  labs(x = "Type(Class) Played",
       y = "Win Rate (WR)",
       title = "Stats by Type Played")

gridExtra::grid.arrange(
  g1,
  g2,
  ncol = 2
)
