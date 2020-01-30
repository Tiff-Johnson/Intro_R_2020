# GGplot
# Mapping on day 3
# 30 January 2020
# Tiffany Johnson

# Load packages
library(tidyverse)
library(boot)

# Plotting with boot
urine <- boot::urine
view(urine)

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond)) +     # colour = cond, adding another variable to the graph with a colour gradient
  labs(x = "Osmoregulation", y = "pH")

## Mapping in R

# Load libraries 
library(tidyverse)
library(ggpubr)

# Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
#load("data/MUR.RData")
load("data/MUR_low_res.RData")


# Custom made colour pallete
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "maroon1", fill = "maroon2", aes(group = group)) # The land mask


# Borders
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders


ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent # expand map past limit


#change name so all resolutions are under sst
sst <- MUR_low_res


## Temperature gradient shown (bins)
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures, raster = group og images put togther to make one image
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

cols69 <- c("#92A547","#6DA55A","#4AA370","#2A9E87","#239699","#3F8CA3","#6180A4","#7E739C","#93658A","#9F5974")

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols69) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
labs(x = "Longidude", y = "Latitude")

## Final Map

final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # temperatures btw certain ranges have diff colour bins
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "yellow"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map


# Chapter 10

#loading packages
library(tidyverse)
library(scales)
library(ggsn)
library(maps)

load("data/africa_map.RData") # use load function when the data is in RData format

ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 


sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1


sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", # annotate = want to add in words, edit plot
           x = 15.1, y = -32.0, 
           size = 5.0, 
           angle = 30 , 
           colour = "navy") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 60, 
           colour = "springgreen") +
theme_bw()
sa_2


sa_3 <- sa_2 +
  # scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
   #         dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
    #        transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 30, x.max = 33, y.min = -37, y.max = -34, # Set location of symbol
        scale = 1.2, symbol = 2)
sa_3










