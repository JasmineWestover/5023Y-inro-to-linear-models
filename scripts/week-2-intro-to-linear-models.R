# Install Packages ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

## 13.1 A linear model analysis for comparing groups ----
# general function for fitting linear models, part of base R
# "least-squares model 0" example being used below, the linear-model uses a techniques called least-squares
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

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

