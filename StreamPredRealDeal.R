library(readr)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(janitor)

# Loading the data

data <- read_csv("past_time_sample_data.csv") %>% 
  select(-X9,-X10) 

CaveCreek <- data %>% 
  filter(data$Stream=="Cave Creek")

WestTurkey <- data %>% 
  filter(data$Stream=="West Turkey")

EastTurkey <-data %>% 
  filter(data$Stream=="East Turkey")

# Reorganizing Cave Creek data

CaveCreek$Site <- factor(CaveCreek$Site,      # Reordering group factor levels
                      levels = c("Research Station","John Hands","Herb Martyr", "Site 9", "Site 10"))

CaveCreek %>% ggplot(aes(x=Species,y='Specimens/m^2'))+
  facet_grid(cols = vars(Date))+
  geom_col(aes(color=Species, fill=Species))+
  # geom_text(aes(label='Specimens/m^2'), position = position_stack(vjust = 0.5),size=3.5)+
  # theme_linedraw()+
  labs(x="",
       y="Individual Specimens")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
