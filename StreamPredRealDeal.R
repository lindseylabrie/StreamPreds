library(readr)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(janitor)
library(ggpubr)
library(brms)

# Loading the data
# looking ONLY at hellgrammites, rrs (red rock skimmer), and oplonaeschna (riffle darner)

# getting the data for the years that are not 2022
data <- read_csv("past_time_sample_data.csv") %>% 
  clean_names() %>% 
  mutate(timed_sample = case_when(timed_sample=="Timed 1"~"1",
                   timed_sample=="Timed 2"~"2",
                   timed_sample=="1"~"1"),
         altitude = case_when(site=="John Hands"~"1715",
                              site=="Herb Martyr"~"1760",
                              site=="Research Station"~"1615",
                              site=="Site 9"~"1825",
                              site=="Site 10"~"1900")) %>% 
  filter(species %in% c("hellgrammite", "oplonaeschna", "RRS"),
         date %in% c("2013","2015","2016")) %>% 
  select(-x9, -x10)
           
           
# 2022 combined timed sample data
combined_timed <- read_csv("combined_time_sample_data.csv") %>% 
  clean_names() %>% 
  filter(species %in% c("hellgrammite", "oplonaeschna", "RRS")) %>% 
  mutate(timed_sample = case_when(timed_sample=="1 and 2 combined"~"3 - Average"),
         altitude = case_when(site=="John Hands"~"1715",
                              site=="Herb Martyr"~"1760",
                              site=="Research Station"~"1615",
                              site=="Site 9"~"1825"))

all_timed <- bind_rows(data, combined_timed)
write.csv(all_timed, "timed_data_from_kick_samples.csv")

# the other bugs
other_timed <- read_csv("combined_time_sample_data.csv") %>% 
  clean_names() %>% 
  filter(species %in% c("caddisflies", "bellastomata", "water fleas", "damselfly","predatory beetles")) %>% 
  mutate(timed_sample = case_when(timed_sample=="1 and 2 combined"~"3 - Average"),
         species = case_when(species=="bellastomata"~"belostomatid",
                             species=="caddisflies"~"caddisflies",
                             species=="water fleas"~"water fleas",
                             species=="damselfly"~"damselfly",
                             species=="predatory beetles"~"predatory beetles"))


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
  facet_grid(rows=vars(species))+
  labs(title = "Cave Creek Sites, 2013",
       y="Specimens per Square Meter")

CC2015 %>% ggplot(aes(x=site, y=specimens_m_2,color=species))+
  geom_col(aes(fill=species), position = position_dodge(width=1))+
  facet_grid(rows=vars(species))+
  labs(title = "Cave Creek Sites, 2015",
       y="Specimens per Square Meter")

CC2016 %>% ggplot(aes(x=site, y=specimens_m_2,color=species))+
  geom_col(aes(fill=species), position = position_dodge(width=1))+
  facet_grid(rows=vars(species))+
  labs(title = "Cave Creek Sites, 2016",
       y="Specimens per Square Meter")


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

other_preds_compared <- other_timed %>% ggplot(aes(x=site,y=specimens_m_2))+ 
  facet_grid(rows = vars(species))+
  geom_col(aes(fill=species))+
  geom_text(aes(label=specimens_m_2), position = position_nudge(y=0.05),size=4)+
  # theme_linedraw()+
  labs(title = "Raw data comparison of density across sites in Cave Creek",
       subtitle = "Other stream predators",
       x="",
       y="Density of Specimens per Square Meter")+
  theme(axis.text.x = element_text(size=12, angle=45, vjust= 1, hjust = 1),
        axis.text.y = element_text(size=12),
        legend.position = "none")

ggsave(other_preds_compared, file= "plots/other_preds_compared.png", dpi = 350, width = 6, height = 8, units = "in")

main_preds <- CC2022 %>% ggplot(aes(x=site,y=specimens_m_2))+ 
  facet_grid(rows = vars(species))+
  geom_col(aes(fill=species))+
  geom_text(aes(label=specimens_m_2), position = position_nudge(y=0.05),size=4)+
  # theme_linedraw()+
  labs(title = "2022, Raw data comparison of density across sites in Cave Creek",
       subtitle = "Main Stream Predators",
       x="",
       y="Density of Specimens per Square Meter")+
  theme(axis.text.x = element_text(size=12, angle=45, vjust= 1, hjust = 1),
        axis.text.y = element_text(size=12),
        legend.position = "none")

ggsave(main_preds, file= "plots/main_preds.png", dpi = 350, width = 6.25, height = 8, units = "in")



#### some sort of analysis , start from "getting priors" section####

# density over altitude changes

altitude <- specimens_m_2 ~ altitude
class(altitude)

boxplot(specimens_m_2 ~ altitude*species, 
        data = CC2022,
        color = "species",
        varwidth = TRUE)

summary(aov(specimens_m_2 ~ altitude),
        data = CC2022)

# getting priors
get_prior(specimens_m_2 ~ species + altitude + species*altitude, 
          data = all_timed,
          family = gaussian())

# making my model
altitude <- brm(specimens_m_2 ~ species + altitude + species*altitude, 
                           data = all_timed,
                           family = gaussian(),
                           cores = 1, chains = 1, iter = 1000,
                           sample_prior = "yes",
                           file="models/altitude.rds",
                           file_refit = "on_change")

# conditional effects, taking all individuals into account
plot(conditional_effects(altitude, re_formula = NULL), points = T)
# conditional effects, showing the mean difference
plot(conditional_effects(altitude), points = T)

pp_check(altitude)
pp_check(altitude, type = "hist")

saveRDS(altitude, "models/altitude.rds")

# # conditional effects, manual plotting
# as_draws_df(length_bsr_negbinom)
# 
# cond_effect_length <- conditional_effects(length_bsr_negbinom)
# cond_effect_length$length_s
# 
# cond_effect_length$lenth_s %>% 
#   ggplot(aes(x=length_s)) +
#   geom_pointrange(aes(y=estimate__, ymin=lower__, ymax=upper__))+
#   geom_point(data = length_bsr_negbinom$data, aes(x=length_s, y=combined_egg_total))+
#   theme_default()
# # it keeps saying "estimate__ not found"
# 
# cond_data_length <- length_bsr_negbinom$data %>% distinct(length_s, combined_egg_total)
# 
# posts_length <- add_epred_draws(length_bsr_negbinom, newdata= length_bsr_negbinom$data %>% 
#                                   distinct(length_s) , re_formula = NA)
# 
# 
# posts_length_all <- add_predicted_draws(length_bsr_negbinom, newdata= length_bsr_negbinom$data %>% 
#                                           distinct(length_s) , re_formula = NA)
# 
# d_length <- d %>% distinct(length_mm, length_s)
# 
# PosteriorLength <- posts_length_all %>%
#   group_by(length_s) %>% 
#   left_join(d_length) %>% 
#   median_qi(.prediction) %>% 
#   mutate(length_mm = (length_s*sd(d$length_mm)) + mean(d$length_mm)) %>% 
#   ggplot(aes(x = length_mm, y = .prediction)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = .lower, ymax = .upper),
#               alpha = 0.2) +
#   geom_point(data = d, 
#              aes(y = combined_egg_total)) +
#   labs(title= "Blue Sucker Fecundity Prediction",
#        subtitle="Large grey bar incorporates the variation in individuals",
#        x="Length (mm)",
#        y="Predicted total egg count")
# 
# ggsave(PosteriorLength, file = "plots/PosteriorLength.png", dpi = 750, width = 7, height = 5,
#        units = "in")
# # This model incorporates all individuals, and not JUST the mean.
# 
# PosteriorLengthMean <- posts_length %>%
#   group_by(length_s) %>% 
#   left_join(d_length) %>% 
#   median_qi(.epred) %>% 
#   mutate(length_mm = (length_s*sd(d$length_mm)) + mean(d$length_mm)) %>% 
#   ggplot(aes(x = length_mm, y = .epred)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = .lower, ymax = .upper),
#               alpha = 0.2) +
#   geom_point(data = d, 
#              aes(y = combined_egg_total)) +
#   labs(title= "Blue Sucker Mean Fecundity Prediction",
#        subtitle="Grey bar incorporates only the variation in the mean egg count",
#        x="Length (mm)",
#        y="Predicted total egg count")
# 
# ggsave(PosteriorLengthMean, file = "plots/PosteriorLengthMean.png", dpi = 750, width = 7, height = 5,
#        units = "in")
# 
# 
# 
# 
# 
# 
# 
