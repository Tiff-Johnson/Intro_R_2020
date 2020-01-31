# Day 4 
# 31 January 2020
# Tiffany Johnson
# Tidy data

# load data

load("data/SACTN_mangled.RData") # RData are binary files and cannot be read by any other program, if data set is too big save as R file and not csv. Contains more info of data than csv



library(tidyverse)
library(ggpubr)

# SACTN1-------------------------------------- 

# plotting the data
 
ggplot(data = SACTN1, aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) +   # Paste0 groups togther two variables within the bracket
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  theme_bw()

# SACTN2------------------------------------

# not tidy data, why? source needs to be in one column

SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") # gather = joins columns


# SACTN3---------------------------------------

# var column is not a thing. Depth and temp within var should be two sepearte columns

SACTN3_tidy1 <- SACTN3 %>% 
  spread(key = var, value = val)  # splits data in column that should be in multiple

# SACTN4a-----------------------------------
# Site and source are under the same column "Index"

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") # sep = what sepeartes the two variables in the column in this case it is /

# splitting the date column

SACTN_tidy2 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date))





















