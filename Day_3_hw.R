# Day 3 homework
# 30 January 2020
# Tiffany Johnson


# Exercise 8.5 Facetting

library(tidyverse)
library(ggpubr)

# Load data

laminaria <- read_csv("data/laminaria.csv")
ecklonia <- read_csv("data/ecklonia.csv")

# ecklonia 1st facet image


trial1 <- ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_mass, colour = site)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~site, ncol = 2) + # This is the line that creates the facets, ncol = number of graphs next to each other
  labs(x = "Stipe Length (m)", y = "Stipe Mass (g)") +
theme_dark()

trial1

# ecklonia 2nd facet image

trial2 <- ggplot(data = ecklonia, aes(x = stipe_diameter, y = stipe_length, colour = site)) +
  geom_point() +
  geom_smooth(method = "gam") + # Note the `+` sign here
  facet_wrap(~site, ncol = 2) + # This is the line that creates the facets, ncol = number of graphs next to each other
  labs(x = "Stipe Diameter", y = "Stipe Length") +
  theme_dark()

trial2

# Joining facet images of ecklonia

ggarrange(trial1, trial2,
          ncol = 1, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE, legend = "bottom")  # Create common legend




# Laminaria

laminaria_site <- laminaria %>% 
  group_by(site) %>%
  summarise(mean_total_length = mean(total_length)) %>% 
filter(site == "A-Frame") %>% 
filter(site == "Betty's Bay")
  


laminaria_box <- ggplot(data = laminaria_site, aes(x = site, y = mean_total_length)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "Site", y = "Mean Total length")

laminaria_box






