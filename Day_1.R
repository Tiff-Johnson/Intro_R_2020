# Day 1
# Data, laminaria, statistical analyses 
# 28 January 2020

# Loading packages
library(tidyverse)

laminaria <- read_csv("data/laminaria.csv")

## Viewing the data

head(laminaria) # First 6 rows
tail(laminaria) # Last 6 rows
glimpse(laminaria)  
view(laminaria) # Open the data
names(laminaria) # Column names

# Tidyverse

lab_sub <- laminaria %>% # Tell R which dataframe we are using
  select(site, total_length) # Select only specific columns

lam_splice <- laminaria %>% 
  select(site, total_length) %>% # Select specific columns first
  slice(56:78)

lam_kom <- laminaria %>%
  filter(site == "Kommetjie")


laminaria %>% # Tell R which dataset to use
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie
  nrow() # Count the number of remaining rows













