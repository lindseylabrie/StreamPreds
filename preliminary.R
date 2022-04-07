library(readr)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(janitor)

streampred_datasheet <- read_csv("streampred_datasheet.csv")

d <- streampred_datasheet %>% 
  filter(!is.na(Date)) %>% 
  filter(Species %in% c("rrs","damselfly","belastomata","oplonaeschna","hellgrammite")) %>% 
  mutate(`Site Name`=case_when(`Site Name`=="Site 9"~"Site 9 - 1790 m",
                               `Site Name`=="John Hands"~"John Hands - 1715 m",
                               `Site Name`=="Herb Martyr"~"Herb Martyr - 1770 m",
                               `Site Name`=="Research Station"~"Research Station - 1650 m"),
         Species=case_when(Species=="belastomata"~"Belastomatids",
                           Species=="rrs"~"P. lineatipes",
                           Species=="oplonaeschna"~"O. armata",
                           Species=="hellgrammite"~"Hellgrammites",
                           Species=="damselfly"~"Damselflies"))

# overall head capsule widths
d %>% 
  filter(!is.na(head_capsule_width_mm)) %>% 
  ggplot(aes(x=species, y=head_capsule_width_mm))+
  geom_point()

# overall body lengths
d %>% 
  filter(!is.na(body_length_cm)) %>% 
  ggplot(aes(x=species, y=body_length_cm))+
  geom_point()

# hellgrammite size distribution
d %>% filter(species=="hellgrammite") %>% 
  ggplot(aes(x=site_name, y=head_capsule_width_mm))+
  scale_x_discrete(limits=c("Research Station - 1650 m","John Hands - 1715 m","Herb Martyr - 1770 m", "Site 9 - 1790 m"))+
  geom_point()+
  labs(title="hellgrammite head size distribution",
       x="site name",
       y="head capsule width (mm)")

# oplonaeschna size distribution
d %>% filter(species=="oplonaeschna") %>% 
  ggplot(aes(x=site_name, y=body_length_cm))+
  scale_x_discrete(limits=c("Research Station - 1650 m","John Hands - 1715 m","Herb Martyr - 1770 m", "Site 9 - 1790 m"))+
  geom_point()+
  labs(title="oplonaeschna size distribution",
       x="site name",
       y="body length (cm)")

# belastomata size distribution
d %>% filter(species=="belastomata") %>% 
  ggplot(aes(x=site_name, y=body_length_cm))+
  scale_x_discrete(limits=c("Research Station - 1650 m","John Hands - 1715 m","Herb Martyr - 1770 m", "Site 9 - 1790 m"))+
  geom_point()+
  labs(title="belastomata size distribution",
       x="site name",
       y="body length (cm)")

# damselfly size distribution
d %>% filter(species=="damselfly") %>% 
  ggplot(aes(x=site_name, y=body_length_cm))+
  scale_x_discrete(limits=c("Research Station - 1650 m","John Hands - 1715 m","Herb Martyr - 1770 m", "Site 9 - 1790 m"))+
  geom_point()+
  labs(title="damselfly size distribution",
       x="site name",
       y="body length (cm)")

# red rock skimmer size distribution
d %>% filter(species=="rrs") %>% 
  ggplot(aes(x=site_name, y=body_length_cm))+
  scale_x_discrete(limits=c("Research Station - 1650 m","John Hands - 1715 m","Herb Martyr - 1770 m", "Site 9 - 1790 m"))+
  geom_point()+
  labs(title="red rock skimmer size distribution",
       x="site name",
       y="body length (cm)")

# want to compare abundance of all 5 species at each site.
# x axis has sites, y axis has species counts for just those 5 species.

Site_9 <- d %>% 
  filter(site_name=="Site 9")

new_d <- d %>% group_by(`Site Name`,Species) %>% count() %>% 
  mutate(counts=n)

new_d$site_name <- factor(new_d$`Site Name`,      # Reordering group factor levels
                         levels = c("Research Station - 1650 m","John Hands - 1715 m","Herb Martyr - 1770 m", "Site 9 - 1790 m"))

AltitudePlot <- new_d %>% ggplot(aes(x=Species,y=counts))+facet_grid(cols = vars(site_name))+
  geom_col(aes(color=Species, fill=Species))+
  geom_text(aes(label=n), position = position_stack(vjust = 0.5),size=3.5)+
  theme_linedraw()+
  labs(x="",
       y="Individual Specimens")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggsave(AltitudePlot, file = "plots/AltitudePlot.png", dpi=750,  width = 8.5, height = 4,
       units = "in")

