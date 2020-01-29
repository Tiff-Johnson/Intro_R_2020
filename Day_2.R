# Summary stats
# 29 January 2020
# Tiffany Johnson
###################

# Load packages
library(tidyverse)

# Loading in the data

laminaria <- read_csv("data/laminaria.csv")

laminaria %>% # Chose the dataframe
  summarise(avg_bld_wdt = mean(blade_length)) # Calculate mean blade length, first words in brackets is new name

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length)) # Create a summary of the sd of the total lengths




trial <- laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
 group_by(site) %>% 
   summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length), # Create a summary of the sd of the total lengths
            median_stp_ln = median(total_length),
            var_stp_ln = var(total_length))



## Plotting - function ggplot

ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 13, colour = "purple", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")


# Plotting

# Load data
ChickWeight <- datasets::ChickWeight

# Create a basic figure
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + #inputting chivk weight data
  geom_point() +  # creating point graph
  geom_line(aes(group = Chick)) # linking this with a line for each chick


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick))


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Create a linear model (line that has best relationship)
theme_grey()


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) +
  geom_smooth(method = "lm", size = 1.2)


# Faceting
library(tidyverse)
library(ggpubr)


# Create faceted figure
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets, ncol = number of graphs next to each other
  labs(x = "Days", y = "Mass (g)")


ChickLast <- ChickWeight %>% 
  filter(Time == 21)


line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1  # highlight graph name and ctrl enter to view saved graphs


lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")

lm_1


# Note that we are using 'ChickLast', not 'ChickWeight'
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")

histogram_1


# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")

box_1

ggarrange(line_1, lm_1, histogram_1, box_1, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend




## HOMEWORK
# Exercise 6

# Plotting

library(tidyverse)

# editing labels, legend, line thickness etc
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(colour = "yellow", fill = "yellow", shape = 5) +
  geom_line(aes(group = Chick), size = 1.5)+
  labs(x = "Time", y = "Weight", colour = "Diet Type") +
  theme(legend.position = "bottom") # Change the legend position

# Load data
Airquality <- datasets::airquality

# Create a basic figure
 
ggplot(data = Airquality, aes(x = Temp,y = Wind, colour = Month)) + #inputting airquality data
  geom_point(size = 1, color = "blue", shape = 23, fill = "green")+    # creating point graph
  geom_line(aes(group = Month))
  
# grouping by month to calculate avg temp per month
AQ_temp_month <- Airquality %>% 
group_by(Month) %>%                 
  summarise(avg_temp = mean(Temp))

# creating avg temp per month graph
ggplot(data = AQ_temp_month, aes(x = Month, y = avg_temp)) + #inputting temp and month data
  geom_col(colour = "purple", fill = "purple") +   # creating bar graph
theme_classic2() +
labs(x = "Month (May - September)", y = "Average Temperature")
  
## creating line graph displaying wind vs temp grouped by Months  
ggplot(data = Airquality, aes(x = Temp,y = Wind, colour = Month)) + #inputting airquality data
  geom_point(size = 1, color = "blue", shape = 23, fill = "green")+    # creating point graph
  geom_line(aes(group = Month))  
  
  
 



















