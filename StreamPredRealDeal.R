library(readr)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(janitor)

# Loading the data
# looking ONLY at hellgrammites and oplonaeschna
data <- read_csv("past_time_sample_data.csv") %>% 
  select(-X9,-X10) %>% 
  filter(data$`Timed Sample`==c("1", "Timed 1"))

CaveCreek <- data %>% 
  filter(data$Stream=="Cave Creek")

WestTurkey <- data %>% 
  filter(data$Stream=="West Turkey")

EastTurkey <-data %>% 
  filter(data$Stream=="East Turkey")

# Reorganizing Cave Creek data

CaveCreek$Site <- factor(CaveCreek$Site,      # Reordering group factor levels
                      levels = c("Research Station","John Hands","Herb Martyr", "Site 9", "Site 10"))

# Visualizing the data over the years 
# trying to see the differences in SITES, YEARS, and DENSITIES the same time

# Looking at data by year

CC2022 <- CaveCreek %>% filter(CaveCreek$Date==2022)
CC2013 <- CaveCreek %>% filter(CaveCreek$Date==2013)
CC2015 <- CaveCreek %>% filter(CaveCreek$Date==2015)
CC2016 <- CaveCreek %>% filter(CaveCreek$Date==2016)

# Seeing if there's a difference between Timed 1 and Timed 2
CC2022 %>% ggplot(aes(x=Site, y=`Specimens/m^2`))+
  geom_point(aes(color=Species))+
  facet_grid(cols = vars(`Timed Sample`))
# kind of looks like there is a difference between timed one and two,
# but would need to make a model to see if that is true.

CC2013 %>% ggplot(aes(x=Site, y=`Specimens/m^2`))+
  geom_point(aes(color=Species))

CC2015 %>% ggplot(aes(x=Site, y=`Specimens/m^2`))+
  geom_point(aes(color=Species))

CC2016 %>% ggplot(aes(x=Site, y=`Specimens/m^2`))+
  geom_point(aes(color=Species))

CaveCreek %>% ggplot(aes(x=Site,y=`Specimens/m^2`))+
  # facet_grid(cols = vars(Date))+
  geom_col(aes(color=Site, fill=Species))+
  # geom_text(aes(label='Specimens/m^2'), position = position_stack(vjust = 0.5),size=3.5)+
  # theme_linedraw()+
  labs(x="",
       y="Density of Specimens per Square Meter")+
  theme_light()

