# Install Packages ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

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

# 13.3 Confidence Intervals ----

# can once again either a base R function or a tidyverse one to calculate the confidence intervals
confint(lsmodel1) # base R
broom::tidy(lsmodel1, conf.int=T) # tidyverse
# the bounds provided were 2.5% and 97.5% of a t-distribution

# 13.4 Answering the Question -------

# Original Hypothesis = self-pollination reduces fitness (height)
# Null hypothesis = no effect of pollination & no difference in average heights. Can we accept or reject this based on analysis & with what level of confidence?
# If the null hypothesis' predicted value lies within the 95% CI for the difference of the mean, we can decide whether to reject or not
# The upper & lower bounds of the confidence intervals do not cross 0
# The difference in height is consistent with Darwin's  alternate hypothesis of inbreeding depression.

# the below function, produces a graph of the estimated mean difference with an approx 95% CI
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.99)      # edited to 0.99 from 0.95

# can also include this argument in the tidy() function if we wish to:
broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

## 13.4.1 Getting the other treatment mean & standard error -----

# to calculate the "other" mean and SE for the self treatment can use the function below
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

## 13.4.2 Emmeans ----

# can use the emmeans function to do a similar thing as mentioned above
means <- emmeans::emmeans(lsmodel1, specs = ~ type)
means

# this provides the mean, SE & 95% CI estimates of all levels at once from the model. 
# seen below is another use for emmeans, where it can provide a summary to include in data visuals. 
means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

# 13.5 Assumption Checking ----

# our linear model is a GOLEM- it does what I tell it to do- the golem follows assumptions and we have to check whether these assumptions are being adequately met.
# Assumptions = data is normally distributed & residual variance is approx. equal between groups.
# Once again we can either use an R based function or a tidyverse function to check the assumptions of linear models
performance::check_model(lsmodel1)  # Base R function to test assumptions
plot(lsmodel1)                      # Tidyverse function to test assumptions

## 13.5.1 Normal Distribution ------
performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))

### 13.5.1.1 What is Quantile-Quantile (QQ) plot? ----
# QQ plot is used to check whether a sample distribution is the same as another. 
# The qqplot distributes the data on the y-axis, & theoretical normal distribution on the x-axis.
# If the residuals follow a normal distribution, they should meet to produce a perfect diagonal line across the plot. 
# Most residuals can be explained by a normal distribution, except at the extreme low end of our data.
# Fit not perfect, but not terrible.

## 13.5.2 Equal Variance ----
performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

# To assess if variances are equal we can plot the residuals (variance) of our data against the fitted (predicted) values.
# Residuals = 0, there would be no error, and our data exactly matches the estimates. This realistically does not happen.
# the check_models plot provides 'standardized residuals' - where we divide the residual error by the standard deviation.
# higher fitted values (Cross treatment) appears to be more variable than the lower fitted values.

## 13.5.3 Outliers ----
# we can check how much of an effect the outliers might be having on the model estimates (means).
performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

# The value which the data points are being measured against is called Cook's distance. 
# Cook's Distance = how much 'leverage' a single data point is exerting on the model, if it is too high, it may be having an outsized effect on the estimates.

# 13.6 Summary ----
# Our model isn't perfect, but it is reasonably good. 
# In this example we used a students t-test, not a paired one (will be implemented later - the plants are paired)
# lm sets one factor as 'intercept' estimates the mean, and the line is the difference in means between the two treatments. 
# The difference in means is always accompanied by a SED, which can be used to calculate a 95% confidence interval.
# If this confidence interval does not contain the intercept value, we can reject the null hypothesis that there is 'no effect'.

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
