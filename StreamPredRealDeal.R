# load libraries
library(Matrix)
library(dbplyr)
library(tidyverse)
library(brms)
library(dplyr)
library(janitor)
library(readr)
library(tidybayes)
library(ggplot2)

# Loading the data
# looking ONLY at hellgrammites, rrs (red rock skimmer), and oplonaeschna (riffle darner)

# getting the data for the years that are not 2022
data <- read_csv("past_time_sample_data.csv") %>% 
  clean_names() %>% 
  mutate(timed_sample = case_when(timed_sample=="Timed 1"~1,
                   timed_sample=="Timed 2"~2,
                   timed_sample=="1"~1),
         altitude = case_when(site=="John Hands"~1715,
                              site=="Herb Martyr"~1760,
                              site=="Research Station"~1615,
                              site=="Site 9"~1825,
                              site=="Site 10"~1900)) %>% 
  filter(species %in% c("hellgrammite", "oplonaeschna", "RRS"),
         date %in% c("2013","2015","2016")) %>% 
  select(-x9, -x10)
           
           
# 2022 combined timed sample data
full_2022_kicks <- read_csv("full_2022_kicks.csv") %>% 
  clean_names()  %>% 
  filter(!is.na(date)) %>% 
  rename(species=species_5,
         site=site_3) %>% 
  mutate(altitude = case_when(site=="John Hands"~1715,
                              site=="Herb Martyr"~1760,
                              site=="Research Station"~1615,
                              site=="Site 9"~1825)) %>% 
  filter(species %in% c("hellgrammite", "oplonaeschna", "RRS"))

all_timed <- bind_rows(data, full_2022_kicks) %>% 
  mutate(specimens_m_2_01 = specimens_m_2 + 0.001,
         altitude_km = altitude/1000)

write.csv(all_timed, "timed_data_from_kick_samples.csv")

# the other bugs
other_timed <- read_csv("full_2022_kicks.csv") %>% 
  clean_names() %>% 
  rename(species=species_5,
         site=site_3) %>% 
  filter(species %in% c("caddisflies", "bellastomata", "water fleas", "damselfly","predatory beetles")) %>% 
  mutate(species = case_when(species=="bellastomata"~"belostomatid",
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
  facet_grid(cols=vars(timed_sample)) +
  labs(title= "2022 Timed Sample Densities",
       color = "Species")+
  geom_text(aes(label=specimens_m_2), position = position_nudge(x=0.01,y=0.01),size=5, color="black")+
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=14, angle=45, vjust= 1, hjust = 1),
        axis.text.y = element_text(size=14),
        )
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
  facet_grid(rows = vars(species), cols = vars(timed_sample))+
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
  facet_grid(rows = vars(species), cols=vars(timed_sample))+
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



#### some sort of analysis , start from "getting priors" section ####

# getting priors
get_prior(specimens_m_2 ~ 1 + altitude*date + (1|species), 
          data = all_timed,
          family = Gamma(link="log"))

# simulating priors
priors = tibble(altitude_beta = rnorm(100, 0, 1),
                Intercept = rnorm(100, -7, 3),
                iter = 1:100)

prior_sims = priors %>%
  expand_grid(all_timed %>% distinct(altitude, date)) %>%
  mutate(date_alt = paste0(date, "_", altitude),
         date_no = date - 2013,
         date_factor = case_when(date_no == 0 ~ 0, TRUE ~ 1),
         date_alt_factor = case_when(date_alt == "2013_1615" ~ 0,
                                    TRUE ~ 1)) %>% 
  mutate(density_sims = exp(Intercept + altitude_beta*(altitude/1000) +
                              altitude_beta*date_factor + altitude_beta*date_alt_factor))


library(scales)

ggplot() +
  geom_point(data=prior_sims, aes(x = altitude, y = density_sims, group = iter),
             alpha = 0.25) +
  scale_y_log10(label = comma) +
  facet_grid(~date)

geom_point(data=all_timed,aes(x=altitude, y=log(altitude_beta),color="species"))


# change altitude_km to site
# making my model
altitude <- brm(specimens_m_2_01 ~ 1 + date + (1 + date|species) + (1 + date|site), 
                           data = CaveCreek,
                           family = Gamma(link="log"),
                prior = c(prior(normal(0,1), class = "b"),
                          prior(normal(-7,3), class="Intercept")),
                           cores = 4, chains = 4, iter = 1000,
                           sample_prior = "no")

# altitude_update <- update(altitude, sample_prior="no", iter=2000, chains=4, cores=4)

summary(altitude)

hist(all_timed$specimens_m_2)

# conditional effects, taking all individuals into account
plot(conditional_effects(altitude, re_formula = NA), points = T)

# conditional effects, showing the mean difference
plot(conditional_effects(altitude), points = T)

pp_check(altitude)
pp_check(altitude, type = "hist")

saveRDS(altitude, "models/altitude.rds")

# conditional effects, manual plotting

posts <- altitude$data %>% 
  select(date, species) %>% 
  expand_grid(site = unique(altitude$data$site)) %>% 
  add_epred_draws(altitude, re_formula = NULL)


Predictions <- posts %>% 
  ggplot(aes(x = site, y = .epred)) + 
  geom_violin(aes(group = site), fill="honeydew3") +
  facet_grid(species~date) +
  geom_point(data=altitude$data, aes(y = specimens_m_2_01), color="darkolivegreen", fill="darkolivegreen2",shape=21, size=2) + 
  scale_y_log10(label=comma)+
  # scale_y_discrete(0,10)+
  # theme_linedraw()+
  labs(title = "Predicted density across sites in Cave Creek",
       subtitle = "Stream Predators",
       x="",
       y="Density of Specimens per Square Meter (note: log scale)")+
  theme(axis.text.x = element_text(size=10, angle=45, vjust= 1, hjust = 1),
        axis.text.y = element_text(size=10),
        legend.position = "none")

ggsave(Predictions, file= "plots/Predictions.png", dpi = 350, width = 8.5, height = 7, units = "in")

# generalized linear mixed model, structure of model can be run with any statistical framework
# same as logistic regression model (likelihood is not Gaussian)

### running with altitude instead of site ###

# getting priors
get_prior(specimens_m_2 ~ 1 + date + (1 + date|species) + (1 + date|altitude), 
          data = all_timed,
          family = Gamma(link="log"))

# simulating priors
priors2 = tibble(altitude_beta = rnorm(100, 0, 1),
                Intercept = rnorm(100, -7, 3),
                iter = 1:100)

prior_sims2 = priors2 %>%
  expand_grid(all_timed %>% distinct(altitude, date,species)) %>%
  mutate(date_alt = paste0(date, "_", altitude),
         date_no = date - 2013,
         date_factor = case_when(date_no == 0 ~ 0, TRUE ~ 1),
         date_alt_factor = case_when(date_alt == "2013_1615" ~ 0,
                                     TRUE ~ 1)) %>% 
  mutate(density_sims = exp(Intercept +
                              altitude_beta*date_factor + 
                              altitude_beta*date_alt_factor))

ggplot() +
  geom_point(data=prior_sims, aes(x = altitude, y = density_sims, group = iter),
             alpha = 0.25) +
  scale_y_log10(label = comma) +
  facet_grid(~date)


altitude2 <- brm(specimens_m_2_01 ~ 1 + date + (1 + date|species) + (1 + date|altitude), 
                data = CaveCreek,
                family = Gamma(link="log"),
                prior = c(prior(normal(0,1), class = "b"),
                          prior(normal(-6,3), class="Intercept")),
                cores = 1, chains = 1, iter = 1000,
                sample_prior = "no",
                file="models/altitude2.rds",
                file_refit = "on_change")

# altitude_update <- update(altitude, sample_prior="no", iter=2000, chains=4, cores=4)

summary(altitude2)

posts2 <- altitude2$data %>% 
  select(date, species,altitude) %>% 
  expand_grid(altitudes = unique(altitude2$data$altitude)) %>% 
  add_epred_draws(altitude2, re_formula = NULL)


posts2 %>% 
  group_by(altitudes,species) %>% 
  median_qi(.epred) %>% 
  arrange(species)

Predictions2 <- posts2 %>% 
  ggplot(aes(x = altitude, y = .epred)) + 
  geom_boxplot(aes(group = altitude), alpha=0.01) +
  facet_grid(species~date) +
  geom_point(data=altitude2$data, aes(y = specimens_m_2_01), color="darkolivegreen", fill="darkolivegreen4",shape=21, size=2) + 
  scale_y_log10(label=comma)+
  # scale_y_discrete(0,1.25)+
  # theme_linedraw()+
  labs(title = "Predicted density across sites in Cave Creek",
       subtitle = "Stream Predators",
       x="Altitude (km)",
       y="Density of Specimens per Square Meter (note: log scale)")+
  theme(axis.text.x = element_text(size=10, angle=45, vjust= 1, hjust = 1),
        axis.text.y = element_text(size=10),
        legend.position = "none")

ggsave(Predictions2, file= "plots/Predictions2.png", dpi = 350, width = 8.5, height = 7, units = "in")

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
