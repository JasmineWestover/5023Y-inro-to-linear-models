# Install Packages ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

# _______________________________ ####

## 13.1 A linear model analysis for comparing groups ----
# general function for fitting linear models, part of base R
# "least-squares model 0" example being used below, the linear-model uses a techniques called least-squares
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

# _____________________________ ####

# 13.2 Summaries for Models ----
# investigate a summary of the model using summary function (seen below)
summary(lsmodel0)

## 13.2.1 Broom ----
# this summarizes key info about models in tidy tibble(s)
broom::tidy(lsmodel0)  # summarize info about model components
broom::glance(lsmodel0) # reports info about the entire model
broom::augment(lsmodel0) # adds info about individual observations to a dataset and it can be used to model
                         # predictions onto a new dataset
## 13.2.2 Model Summary ----
# Can use either basic R function or a tidyverse function
# Can use the basic R summary function as seen above
# Can use the tidyverse broom tidy function as seen above
# the output provides a table of coefficients
# 18.9 is the estimate of the model coefficient (overall mean) with its standard error.
# we are able to prove that this is the same as the overall mean:
mean(darwin$height)  # answer = 18.88333 (rounded = 18.9)

## 13.2.3 Compare Means ----
# we want a linear model that can analyse the differences in average plant height, can use the lm() function
lsmodel1 <- lm(height ~ type, data=darwin)

# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data=darwin)  the model formula contains the pollination type in addition to an intercept

broom::tidy(lsmodel1) # the second row in the tibble, shows the difference in the mean height of he 2 groups

# This linear model indicates the average height of Crossed plants is 20.2 inches.
# Selfed plants are an average of 2.6 inches shorter, we are able to confirm this using the formula below:

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

# we can use the information provided by the model apply the calculated means onto a plot
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

## 13.2.4 Standard error of the difference -----
# LM output provides the standard errors of the values.
# The first row calculates mean & standard error of the mean(SEM).
# As the second row provides the mean difference- the standard error of the difference between the two means (SED) is provided, which can also be calculated using a formula.

