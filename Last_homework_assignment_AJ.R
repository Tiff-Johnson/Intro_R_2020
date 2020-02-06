# Tiffany Johnson
# 3650499
# Last homework assignmet (AJ) given on 31 Jan
# Tidying data and plotting/mapping



library(tidyverse)
library(scales)
library(ggsn)
library(maps)
library(ggpubr)

load("gebco_sa.RData")

# Exploring the data

glimpse(bathy_wide)
ncol(bathy_wide)  # 1921
nrow(bathy_wide)  # 1500
summary(bathy_wide)


# Tidying the data
# Why is the data not tidy? Longitude values need to be under one column not have there own seperate columns

tail(names(bathy_wide),1)  # Find out name of last column

bathy_tidy <- bathy_wide %>%
  gather("7.99166666665" : "39.97500006395", key = "lon", value = "elevation") # gather = joins columns. Start column : end column

summary(bathy_tidy)

# Mapping the data

# Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")

# making palette
cols20 <- c("#B5B348","#AEB249","#A7B24A","#A0B14B","#9AB04D","#93AF4F","#8CAE51","#86AD53","#7FAC55","#79AA57","#73A95A","#6DA85C","#67A65F","#61A561","#5BA364","#55A166","#4FA069","#4A9E6B","#449C6D","#3F9A70","#399872","#349674","#2F9476","#2B9278","#27907A","#238E7B","#1F8C7D","#1D8A7E","#1B887F","#1A8580","#1B8381","#1C8182","#1E7E82","#207C83","#237983","#267783","#297483","#2D7282","#306F82","#336D81","#376A80","#3A687F","#3D657D")

## Elevation gradient shown

elevation_final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = bathy_tidy, aes(fill = elevation)) + 
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Elevation / Depth (m)", values = cols20) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 10), # Change text size in legend
        legend.title = element_text(size = 10), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(), # Add legend background
        legend.justification = c("right"), # Change position of legend
        legend.position = c()) # Fine tune position of legend
  
elevation_final_map























































