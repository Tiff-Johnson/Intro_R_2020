# Tiffany Johnson
# Last homework assignment - Amieroh
# Intro R Workshop: Data Manipulation, Anlyses and Visualisation
# Tidy data

library(tidyverse)

# Section 1

BOD <- datasets::BOD

# C

BJsales <- datasets::BJsales
EuStockMarkets <- datasets::EuStockMarkets
DNase <- datasets::DNase
Formaldehyde <- datasets::Formaldehyde
Orange <- datasets::Orange
UCBAdmissions <- datasets::UCBAdmissions

# Tidy data sets: DNase, Formaldehyde, Orange

-------------------------------------------------------------------------------------------------------------------------

# Section 2
  
 library(dplyr) 
 library(dslabs) 

 data("murders")  # load data

 view(murders)
 glimpse(murders)  
head(murders)  
tail(murders)  
ncol(murders)
nrow(murders)  
names(murders) # column names 

# The murders dataset lists the states of America in alphabetical order starting from Alabama and ending with Wyoming, in the first column.  
# The second column contains each state's corresponding abbreviation. The variable in the third column are the regions each state can be found in.
# The forth column contains the populations of each state and then forth and final column contains the number of murders recorded in each specific 
# state for the time being observed.

murders <- mutate(murders, population_in_millions = population / 10^6)

murders %>% 
select(state, population)

murders %>% 
filter(state != "Florida")

no_south <- murders %>% filter(region != "South")

NY_TX <- murders %>% 
  filter(state %in% c("New York", "Texas"))

SW_pops <- murders %>% filter(region %in% c("South", "West"))

South <- murders %>%
  filter(region %in% c("South")) 

South %>% 
  summarise(South_pop = sum(population))
# 115674434

West <- murders %>%
  filter(region %in% c("West")) 

West %>% 
  summarise(West_pop = sum(population))
# 71945553

Northeast_pop <- murders %>% 
  filter(region %in% c("Northeast"), population)

Northeast_pop <- Northeast_pop %>% 
  select(population)

# Plotting 
library(ggpubr)

ggplot(data = murders, aes(x = region, y = total)) +
  geom_point() +
  labs(x = "Region", y = "Total Murders")

# In this plot we see that the trend is that the West region has the state with the highest murder rate, followed by the South region.

ggplot(data = murders, aes(x = total, y = population, colour = region)) +
  geom_line() +
  facet_wrap(~region, ncol = 2) +
  labs(x = "Total Murders", y = "population")

# The general trend of this plot is that as the population increases so does the murder rate

# South = 115674434
# West = 71945553
# The South has a much bigger population size compared to the West. The difference between the two region's population is 43 728 881

New_data <- murders %>% 
  filter(total > 20, total < 100, total - 120)

murders_slice <- murders %>% 
  slice(10:24, 26)

murders_tibble <- murders %>% 
  as_tibble(murders)

murders_region <- murders %>% 
  group_by(region) %>% 
as_tibble

-------------------------------------------------------------------------------------------------------------------------

# Section 3
  
library(dplyr) 
library(dslabs)  
  
# load data
  
  data("heights")
  
view(heights) 

# The heights dataset consists of two columns containing two seperate variables. The first column lists the sexes of the students, either male or female.
# The second column contains the heights assigned to the male or female student in the first column.

glimpse(heights)
tail(heights)  
head(heights)  
nrow(heights)  
ncol(heights)
names(heights)

avg_sd <- heights %>% 
  group_by(sex) %>% 
  summarise(avg = mean(height), 
            sd = sd(height))

med_min_max <- heights %>% 
  group_by(sex) %>% 
  summarise(med = median(height),
            min = min(height),
            max = max(height))

-------------------------------------------------------------------------------------------------------------------------------

 # Section 4 
  
  library(plyr)
  
  x <-  c(1, 6, 21, 19 , NA, 73, NA)

  y <-  c(NA, NA, 3, NA, 13, 24, NA)  
  
  view(y)
  view(x)
  
# A
count(x, vars = "NA")
count(y, vars = "NA")

# B
count(vars = "NA")

f <- c(4, 6, NA, 8, NA, 9, NA)
g <- c(NA, 22, 27, NA, NA, NA, 30)
h <- c(77, NA, NA, 78, 79, 80, NA)

f %>% 
  count(vars = "NA") 

g %>% 
  count(vars = "NA")

h %>% 
  count(vars = "NA")

----------------------------------------------------------------------------------------------------------------------------
  
  # Section 5
  
  Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                              winter = c(41, 39, 47, 40),
                              spring = c(41, 46, 57, 45),
                              summer = c(75, 52, 85, 66),
                              Autumn = c(57, 66, 52, 56))


# Hypothesis: Summer will have the highest average temp for each year and winter will have the lowest


ggplot(data = Seasonal_data, aes(x = (winter, spring, summer, Autumn), y = year)) +
  geom_line() 
  labs(x = "Season", y = "Year")


  cats_data<- tibble(cats = c("A", "B", "C"),
                     position = c("1-2-3", "3-1-2", "2-3-1"),
                     minutes = c(3, 3, 3),
                     seconds = c(12, 44, 15))
  cats_data

  cats_data_sep <- cats_data %>% 
    separate(col = position, into = c("first_place", "second_place", "third_place"), sep = "-")

   library(lubridate)
   
   cats_data_unite <- cats_data %>% 
     unite(minutes, seconds, col = "total_time", sep = ":")

---------------------------------------------------------------------------------------------------------------------------
     
     # Section 6
library(boot)
     
   nuclear <- boot::nuclear

   
   section_6_gather <- nuclear %>% 
     gather(t1, t2, key = "t", value = "time") # gather = joins columns, joining the t1 and t2 columns
   
   section_6_spread <- section_6_gather %>% 
     spread(key = t, value = time) # spreading info in two columns into their own seperate columns

   section_6_seperate <- nuclear %>%
     separate(col = date, into = c("minutes", "seconds"), sep = ".") # seperating info in one column into two. Using the "." as a seperator becuase that is what seperates them in the data
     
section_6_joining <- join(nuclear, section_6_seperate, type = "left", match = "all") # joining two data frames together

section_6_arrange <- arrange(nuclear, cost) # arranging data by cost (from lowest to highest)

section_6_select <- nuclear %>% select(cost, date) # selecting only the cost and date columns to ne displayed

section_6_groub_by <- nuclear %>% group_by(cost) # grouping the data y cost

section_6_mutate <- nuclear %>% 
  mutate(cost = cost / 2)      # Using the mutate function to divide the cost column by 2












