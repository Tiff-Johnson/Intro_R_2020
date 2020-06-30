# Biostats 
# Chapter 8 
# Linear regressions

# Answers

# 8a
# units
# units

# 8b 
# 0
# 0
# A. The null hypotheses states that there will be no difference between the y-intercept and 0 and the slope and 0.
# B. P value < 0.05, therefore the linear model fitted to the residuals is accepted.
# F-statistic: 1480 on 1 and 723 DF, p-value: < 2.2e-16
# Because there is a significant relationship between the two variables (age and lung capacity).
# .....


# Calculations

library(tidyverse)
lungs <- read_tsv("C:/Users/User/Documents/Biostats/Biostats data/LungCapData.csv") # read a tab separated value file
head(lungs)
dim(lungs)   # view dimensions of dataframe, number of rows and columns
colnames(lungs) # View column names 

ggplot(data = lungs, aes(x = Age, y = LungCap)) +  # plotting age vs lung capacity, age = independent (x) lungcap = dependent (y)
  geom_point(shape = 2, colour = "purple2") +
  labs(x = "Age", y = "Lung capacity")


cor(lungs$Age, lungs$LungCap)  # pearson's coefficient, R-value

?lm # or,
help(lm)  # help tab, explaining linear model function 

mod <- lm(LungCap ~ Age, data = lungs)
summary(mod)   # fit the model

attributes(mod)  # view attributs of linear model "mod"

mod$coef  # extracting named attribute (coefficient)

ggplot(data = lungs, aes(x = Age, y = LungCap)) +          # adding regression line to age vs lung cap
  geom_point(shape = 1, colour = "yellow3") +
  geom_line(aes(y = mod$fitted.values), colour = "blue2") +
  labs(x = "Age", y = "Lung capacity")

confint(mod)   # confidence intervals
confint(mod, level = 0.90)


anova(mod)  # anova table


# using model to make predictions
# create a df with a column called Age (as per the input data)
pred <- data.frame(Age = c(13, 15, 17))
pred
predict(mod, pred)


head(mod$fitted)   # few first fitted values of linear model
# The fitted values are the values that describe the path of the best fit line


ggplot(lungs, aes(Age, LungCap)) +
  geom_point(shape = 1, colour = "red3") +
  stat_smooth(method = lm, se = TRUE, colour = "blue3", size = 0.2) + # CIs around LM
  labs(x = "Age", y = "Lung capacity")    # standard error


head(mod$residuals)  # head of residuals

ggplot(data = lungs, aes(x = Age)) +     # plot of residuals, only add x  
  geom_point(aes(y = mod$residuals), shape = 1, colour = "red3") +
  labs(x = "Age", y = "Residual")


library(ggfortify)
autoplot(mod, colour = "green", shape = 1, size = 0.2, ncol = 2, which = c(1:2))
#  Testing the assumptions...

autoplot(mod, colour = "green", shape = 1, size = 0.2, ncol = 2, which = c(3, 5))

age <- lungs %>% 
  select(Age)     # creating data frame with just age selected

res <- view(mod$residuals)  # creating data frame with just residuals

age_res <- cbind(age,res)  # combining age and residuals into one data frame

mod_res <- lm(x ~ Age, data = age_res)  # creating linear model for age vs residual data
summary(mod_res) 

# Final question.. Fitting linear regression to residuals

ggplot(data = age_res, aes(x = Age)) +     # plot of residuals, only add x  
  geom_point(aes(y = mod_res$residuals), shape = 1, colour = "green") +
  geom_line(aes(y = mod_res$fitted.values), colour = "blue2") +
  labs(x = "Age", y = "Residual")


# ..... 
ggplot(data = lungs, aes(x = Age)) +     # plot of residuals, only add x  
  geom_point(aes(y = mod$residuals), shape = 1, colour = "green") +
  geom_line(aes(y = mod$fitted.values), colour = "blue2") +
  labs(x = "Age", y = "Residual")











