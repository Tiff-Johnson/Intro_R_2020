# Author: Tiffany Johnson
# Student no: 3650499
# Day 1 homework
# Data, laminaria, 
# 28 January 2020

library(tidyverse)

laminaria <- read.csv("data/laminaria.csv")

lam_site <- laminaria %>%  # Tell R which dataframe we are using
  select(site) # Select only specific columns

lam_half <- laminaria %>% 
  mutate(total_length_half = total_length / 2)

lam_half <- lam_half %>% 
  filter (total_length_half < 100) %>% 
  select (site, total_length_half)

lam_site <- laminaria %>% 
  group_by(site) %>% 
  summarise(mean = mean(blade_length),
            min = min(blade_length),
            max = max(blade_length), n = n())

laminaria_heavy <- laminaria %>% 
filter(stipe_mass == max(stipe_mass, na.rm = TRUE))    
                    
            
