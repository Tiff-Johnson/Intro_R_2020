# Intro to Biostats
# Day 1
# Tiffany Johnson
# 19 Feb 2020
# Chapter 2 and 3

library(tidyverse)

chicks <- as_tibble(ChickWeight)  # load in as table tibble = table
head(chicks)  # view first six rows
tail(chicks)
tail(chicks, n = 2)  # view last rows, but only the last 2
colnames(chicks)   # view column variable names
summary(chicks) # gives certain analysis of data eg median, min etc

# how many weights are available across all Diets and Times?
chicks %>% 
  summarise(length = n())  # gives the amount of observations in the data

length(chicks$weight) # same as above function just in base R and not tidy data

dat1 <- c(23, 45, 23, 66, 13)
mean(dat1)  # calculating mean

chicks %>% 
  summarise(mean_wt = mean(weight))

chicks %>% 
  summarise(mean_wt = round(mean(weight), 1))  # round function = rounding off to 1 decimal in this situation

chicks %>% 
  summarise(mean_wt = sum(weight) / n()) # actual mean calculation

chicks %>% 
  summarise(med_wt = median(weight))

library(e1071)
skewness(faithful$eruptions)

kurtosis(faithful$eruptions)

chicks %>% 
  summarise(sd_wt = sd(weight))

quantile(chicks$weight)

chicks %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))

range(chicks$weight)

chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

range(chicks$weight)[2] - range(chicks$weight)[1]

chicks %>% 
  summarise(range_wt = range(weight)[2] - range(weight)[1])

dat1 <- c(NA, 12, 76, 34, 23)

# Without telling R to ommit missing data
mean(dat1)

mean(dat1, na.rm = TRUE)


grp_stat <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat


library(ggpubr) # needed for arranging multi-panel plots
plt1 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_point(data = grp_stat, aes(x = Diet, y = mean_wt), 
             col = "black", fill = "red", shape = 23, size = 3) +
  geom_jitter(width = 0.05) + # geom_point() if jitter not required
  labs(y = "Chicken mass (g)") + 
  theme_pubr()

plt2 <- ggplot(data = grp_stat, aes(x = Diet, y = mean_wt)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           col = NA, fill = "salmon") +
  geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),
                width = .2) +
  labs(y = "Chicken mass (g)") + 
  theme_pubr()
# position_dodge() places bars side-by-side
# stat = "identity" prevents the default count from being plotted

# a description of the components of a boxplot is provided in the help file
# geom_boxplot()
plt3 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Chicken mass (g)") + 
  theme_pubr()

plt4 <- chicks %>%
  filter(Time %in% c(10, 21)) %>% 
  ggplot(aes(x = Diet, y = weight, fill = as.factor(Time))) +
  geom_boxplot() +
  geom_jitter(shape = 21, width = 0.1) +
  labs(y = "Chicken mass (g)", fill = "Time") +
  theme_pubr()

ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2, labels = "AUTO")


## This is good but for the exams it might b useful to have the plotting code in its own scripts. 
# This will make it easier and faster to make reference to the code rather than scrolling down etc
