# Install Packages ----
library(tidyverse)
library(rstatix)
library(performance)

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
