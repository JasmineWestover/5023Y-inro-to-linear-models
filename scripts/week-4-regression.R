# Install Packages ----
library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

janka <- read_csv(here("data", "janka.csv"))

# Uploaded the janka.csv file into the data folder, and made it to an object for easy access
# The janka data is from the Australian foresty industry, recording the density and hardness of 36 samples of wood from different tree species.

# Below will be the functions needed to ensure that the data is tidy and fits the parameters
glimpse(janka)
head(janka)  
colnames(janka)   # "dens" and "hardness"
janka <- janitor::clean_names(janka)
summary(janka)

# 15.4 Activity 1: Exploratory Analysis ----
# Task: Is there any visual evidence for a linear association between wood density and timber hardness?
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  labs(x = "Density of Wood",
       y = "Hardness of Wood")

# added additional axis labels to properly show what it present in the graph, later a linear model showing the 'line of best fit'will be added
# This shows that the density and the hardness of the wood are positively related, and they have a fairly linear relationship. 

# 15.5 Activity 2: Correlation - Generate Pearson's R -----
# Task- can you work ou the code needed to generate Pearson's R?
cor(janka$dens,janka$hardness, method='pearson') # this is the code I generated after using Google, answer = 0.973345

# Below is the code generated using both base R and the tidyverse from the workbook
with(janka, cor(dens, hardness))  # Base R
# cor() does not have a data option so need to use the with() function

janka %>% 
  cor_test(dens, hardness)  # Tidyverse- this produces a tibble that provides the correlation
# Based on the correlation r = 0.97, we can say that the association between the two variables is strongly positive.

# 15.6 Regression in R ------
janka_ls1 <- lm(hardness ~ dens, data = janka) 

# the linear model will estimate a 'line of best fit. We can add a regression line to ggplots using geom_smooth()
# to produce the regression line- have to specify that a linear model method for line fitting is being used. 
janka%>%
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

## 15.6.1 Summary -----
# can once again use base R or the tidyverse to get a summary of the data
summary(janka_ls1) # Base R

janka_ls1 %>%
  broom::tidy()    # Tidyverse

# The intercept describes the value of y (timber hardness) when x (wood density) = 0.
# Timber hardness can not be -ve, which is what ou value is --> doesn't affect the fit of the line, the regression line can move into impossible value ranges.
# We can prevent this by 'centering' the intercept.
## This happens by subtracting the average value of x from every data point, the intercept (when x = 0) can effectively be right-shifted into the centre of the data.

# 15.7 Activity 3: Mean Centered Regression -----
# Task: use data manipulation to 'center' the values of x then fit a new linear model. 
dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333 

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

# the estimate for row 2 - the effect of density on timber hardness has not changed (57.5)
# the intercept now represents the estimated mean timber hardness for the mean wood density

## 15.7.1 The Second Row ----
# Density = explanatory variable, & slope is estimated against it. 
# regression slope (with SE) = 57.5- timber hardness is predicted to increase by 57.5 on the janka scale for every unit change of density.
# estimated change in the mean is statistically significant- unlikely to observe this relationship if null hypothesis (can't predict timber hardness from wood density) were true.

## 15.7.2 Confidence Intervals ----
# We can produce upper and lower bounds of confidence intervals, this can once again be done using both base R and tidyverse. 
confint(janka_ls1) # Base R - Shows the 2.5% & the 97.5% interval
broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)   # Tidyverse 
# The minimum effect size (at 95% confidence) of density on the janka scale is 52.9.

## 15.7.3 Effect Size ----
# A standardized measure of how strong a relationship is can be created, the value is represented by R^2.
# R^2 can be found in the model summaries (using Base R & Tidyverse)
summary(janka_ls1)  # Base R

janka_ls1 %>% 
  broom::glance()   # Tidyverse
# R^2 = 0.9493

# 15.8 Assumptions ----
# Regression models make ALL the same assumptions as all linear models
# Residuals (context of regression line) = vertical distance between a data point and the fitted value on the line. 
# The broom package augment, generates the predicted value for each data point according to the regression, and calculates the residuals for each data point.

# Base R Code for assumptions below
predict(janka_ls1)
resid(janka_ls1)

# Tidyverse Code for Assumptions below - use the augment package
janka_ls1 %>% 
  broom::augment() %>% 
  head()

# if these values are plotted, with a black regression line and red dashed lines representing the residuals:
augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")

# Use augmented data to understand what residual variance looks like & how it's used to diagnose models. 
# Perfect model; residual values = 0 (incredibly unlikely to ever occur)
# Like to see: 'normal distribution' to the residuals & homogeneity of the residuals

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")

p1+p2+p3

# 15.8 Task - make a function that reduces the amount of code needed ----
# Above code is functional but repetitive- try and reduce amount of code

## 15.8.1 Normal Distribution ----
# We can use model diagnostic plots (R & Tidyverse), these show that it is mostly good, with only a couple of data points outside the confidence intervals.
plot(janka_ls1, which=c(2,2)) # Base R
performance::check_model(janka_ls1, check=c("normality","qq")) # Tidyverse

## 15.8.2 Equal Variance ----
# similar to the p3 plot above - this has the 'raw' residuals as a function of the fitted values. 
# The graphs (Base R & Tidyverse) produced below are the 'standardized residuals' = raw residual/standard deviation.
plot(janka_ls1, which=c(1,3)) # Base R- the (1,3), means that only graphs 1 & 3 are plotted when the script is run
performance::check_model(janka_ls1, check="homogeneity")

# Both plots suggest that he residuals do NOT have constant variance - less confidence in our predictions at high values of density.

## 15.8.3 Outliers ----
# There is just one potential outlier. The positional order in the data frame is 32
plot(janka_ls1, which=c(4,5)) # Base R code- based on 13.5.3 (based on week 2 script)
performance::check_model(janka_ls1, check="outliers") # Tidyverse

# 15.9 Prediction ----
# Using the coefficients of the intercept and the slope we can make predictions on new data.
# The estimates of the intercept and the slope are:
coef(janka_ls1)
# if we have a new wood sample with a density of 65, we can use the equation for a linear regression to predict what the timber hardness of the sample should be.
# a + bx - these values comes from the above command (intercept = a, dens = b, density of new wood sample = x)
-1160.49970 + 57.50667 * 65
# The density =  2577.434

# Rather than doing the above code to work out the values manually, can also use the coefficients of the model directly
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65

# We can use functions like predict and broom:augment
predict(janka_ls1, newdata=list(dens=c(22,35,65))) # Base R
broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22,35,65)))  # Tidyverse
#The numbers in the brackets dens - are the values of hardness we are trying to predict from the density provided

## 15.9.1 Adding Confidence Intervals ----
### 15.9.1.1 Standard Error ----
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)
# This is allowing us to predict the values of timber hardness, , in the same way as the above formulas, but now the standard error are also being calculated

### 15.9.1.2 95% Confidence Intervals ----
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")
# This function (package), allows for the predicted values to be calculated, but now provides the confidence intervals 
# emmeans is a good package for producing quick predictions for categorical data.

# 15.10 Activity 4: Prediction ----
# Task: Can you plot the three new predicted values onto an existing figure to recreate the below?
# need to make a new R object that contains your predictions, work out how to add two dataframes to one plot.

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x="Density", y="Timber Hardness")+
  theme_bw()

