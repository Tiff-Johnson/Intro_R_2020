library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(R11)

# load data

load("data/SACTNmonthly_v4.0.RData")

KZNSB <-SACTNmonthly_v4.0 %>% 
  filter(src == "KZNSB")

KZNSB_date <- KZNSB %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date))

KZNSB_group <- KZNSB_date %>% 
  group_by(site, year) %>% 
  summarise(mean_temp = mean(temp)) 

# Create faceted figure

ggplot(data = KZNSB_group, aes(x = year, y = mean_temp, colour = site))+
  geom_line(aes(group = site), color = "purple") +
  facet_wrap(~site, ncol = 5) +
  scale_x_continuous(breaks=c(1980, 2000))+
  scale_y_continuous(breaks=c(20, 22, 24)) + 
  labs(title = "KZNSB: series of annual means",
    x = "Year",
    y = "Temperature (°C)")

#--------------------------------------------------------------------------------------------------------------------

laminaria <- read_csv("data/laminaria.csv")

FB_filter <-laminaria %>% 
  filter(region == "FB")


FB_filter <- FB_group %>%
  select(site, blade_weight, blade_length)

FB_group <- FB_select %>% 
  group_by(site)



Plot1 <- ggplot(data = FB_group, aes(x = blade_length, y = blade_weight, colour = site))+
  geom_point()+
  geom_line(aes(group = site)) +
  facet_wrap(~site, ncol = 3) +
  scale_x_continuous(breaks=c(100, 125, 150, 175))+
  scale_y_discrete(limits=c("0", "1", "2", "3")) + 
  labs(title = "A crazy graph of some data for False Bay sites",
       x = "Blade length (cm)",
       y = "Blade mass (kg)")+
  scale_colour_brewer(palette = "Accent")

Plot1


# Plor for Roman's Rock is missing because color palette used only has 8 colors chosen, and we have 9 sites.
# Solution = chose different paletter or remove palette 


Plot2 <- ggplot(data = FB_group, aes(x = blade_length, y = blade_weight, colour = site))+
  geom_point()+
  geom_line(aes(group = site)) +
  facet_wrap(~site, ncol = 3) +
  scale_x_continuous(breaks=c(100, 125, 150, 175))+
  scale_y_discrete(limits=c("0", "1", "2", "3")) + 
  labs(title = "A crazy graph of some data for False Bay sites",
       x = "Blade length (cm)",
       y = "Blade mass (kg)")+
  scale_colour_brewer(palette = "Paired")

Plot2

ggarrange(Plot1, Plot2, 
          ncol = 2, nrow = 2, 
          labels = c("A", "B")) 

#---------------------------------------------------------------------------------------------------------------------------------------


ToothGrowth <- datasets::ToothGrowth

trial1 <- ToothGrowth %>% 
  group_by(supp, dose) %>% 
  summarise(sd_len = round(sd(len), 2),
            mean_len = mean(len))

trial1
  


ggplot(data = trial1, aes(x = dose, y = len, fill = supp)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           col = NA, fill = "salmon") +
  geom_errorbar(aes(ymin=len-sd_len, ymax=len+sd_len), width=.2,
                position=position_dodge(.9)) +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)")


df2 <- summary(ToothGrowth, varname="len", 
                    groupnames=c("supp", "dose"))

  df2 <- data.frame(df2)

# Convert dose to a factor variable

df2$dose=as.factor(df2$dose)
head(df2)

df2

p<- ggplot(df2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9)) 
print(p)

p+labs(title="Tooth length per dose", x="Dose (mg)", y = "Length")+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00'))







df <- ToothGrowth
df$dose <- as.factor(df$dose)
head(df)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(ToothGrowth, varname="len", 
                    groupnames=c("supp", "dose"))
# Convert dose to a factor variable
df2$dose=as.factor(df2$dose)
head(df2)


p<- ggplot(df2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9)) 
print(p)
# Finished bar plot
p+labs(x="Dose (mg)", y = "Length")




































