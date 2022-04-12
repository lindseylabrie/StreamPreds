library(readr)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(janitor)

# Loading the data
# looking ONLY at hellgrammites, rrs (red rock skimmer), and oplonaeschna (riffle darner)

# getting the data for the years that are not 2022
data <- read_csv("past_time_sample_data.csv") %>% 
  select(-X9,-X10) %>% 
  clean_names() %>% 
  mutate(timed_sample = case_when(timed_sample=="Timed 1"~"1",
                   timed_sample=="Timed 2"~"2",
                   timed_sample=="1"~"1")) %>% 
  filter(species %in% c("hellgrammite", "oplonaeschna", "RRS"),
         date %in% c("2013","2015","2016"))

# 2022 combined timed sample data
combined_timed <- read_csv("combined_time_sample_data.csv") %>% 
  clean_names() %>% 
  filter(species %in% c("hellgrammite", "oplonaeschna", "RRS")) %>% 
  mutate(timed_sample = case_when(timed_sample=="1 and 2 combined"~"3 - Average"))

all_timed <- bind_rows(data, combined_timed)


CaveCreek <- all_timed %>% 
  filter(stream=="Cave Creek")

WestTurkey <- data %>% 
  filter(stream=="West Turkey")

EastTurkey <-data %>% 
  filter(stream=="East Turkey")

# Reorganizing Cave Creek data to make sure it's ordered correctly by site as altitude increases #

CaveCreek$site <- factor(CaveCreek$site,      # Reordering group factor levels
                      levels = c("Research Station","John Hands","Herb Martyr", "Site 9", "Site 10"))

### Visualizing the data over the years ####
# trying to see the differences in SITES, YEARS, and DENSITIES the same time

# Looking at data by year

CC2022 <- CaveCreek %>% filter(date=="2022")
CC2013 <- CaveCreek %>% filter(date=="2013")
CC2015 <- CaveCreek %>% filter(date=="2015")
CC2016 <- CaveCreek %>% filter(date=="2016")

# Seeing if there's a difference between Timed 1, Timed 2, and the combined timed samples.
CC2022 %>% ggplot(aes(x=site, y=specimens_m_2,color=species))+
  geom_col(aes(fill=species), position = position_dodge(width=1))+
  facet_grid(cols=vars(timed_sample))
# kind of looks like there is a difference between timed one and two,
# but would need to make a model to see if that is true.
# Will most likely want to use timed 3 because it is an average of both timed 1 and 2.

CC2013 %>% ggplot(aes(x=site, y=specimens_m_2,color=species))+
  geom_col(aes(fill=species), position = position_dodge(width=1))+
  facet_grid(rows=vars(species))

CC2015 %>% ggplot(aes(x=site, y=specimens_m_2,color=species))+
  geom_col(aes(fill=species), position = position_dodge(width=1))+
  facet_grid(rows=vars(species))

CC2016 %>% ggplot(aes(x=site, y=specimens_m_2,color=species))+
  geom_col(aes(fill=species), position = position_dodge(width=1))+
  facet_grid(rows=vars(species))


# This shows the 3 biggest species compared over the years 2013, 2015, 2016, and 2022 for ONLY the timed samples.
# Note, the 2022 data shows the average of both timed samples to help in comparision, since other years only have
# one kick sample to compare to. Going forward, data should be more consistent, i.e. there will always be 2
# kick samples and averages can be taken, OR the difference between kick sample 1 and kick sample 2 can be evaluated.
# This might not be the best way to compare data across years, but it is a good start.

density_compared_by_year <- CaveCreek %>% ggplot(aes(x=site,y=specimens_m_2))+ 
  facet_grid(rows = vars(species), cols=vars(date))+
  geom_col(aes(fill=species))+
  geom_text(aes(label=specimens_m_2), position = position_nudge(y=0.05),size=5)+
  # theme_linedraw()+
  labs(title = "Raw data comparison of density across sites in Cave Creek over 9 years",
       x="",
       y="Density of Specimens per Square Meter")+
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=14, angle=45, vjust= 1, hjust = 1),
        axis.text.y = element_text(size=14))

ggsave(density_compared_by_year, file= "plots/density_compared_by_year2.png", dpi = 350, width = 13.75, height = 11, units = "in")
