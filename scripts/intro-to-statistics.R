library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv"))

# Hypothesis trying to answer during data analysis is whether 'inbreeding reduced the fitness of the selfed plants'

# 12.2 Activity 1- Basic Exploratory Data Analysis ----

glimpse(darwin)  # Check the structure of the data set
head(darwin)     # Check whether the data is in a tidy format or not
colnames(darwin) # Check the Variable names in the data set, "pair", "type" and "height"
darwin <- janitor::clean_names(darwin) # This function is used to 'clean' the column names if needed
darwin %>%
  duplicated()%>%
  sum()            # Check whether there is duplication within the data set
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE)) # Check what the maximum and minimum values are
summary(darwin) # Produces a summary of the data

## 12.2.1 Visualization ----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()
# Average height of the 'crossed' plants is greater than that of the 'selfed' plants. 

## 12.2.2 Comparing Groups ----
darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))     # Determines the mean and standard deviation of our groups
# The mean of 'cross' = 20.2, and the mean of 'self' = 17.6

# make a new object so can carry out summary statistics with a table
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# 12.4 Activity 2: Differences ----
# In this section we are estimating the mean heights, mean difference of heights & quantifying confidence about the differences
# Need to create a new column called difference with the height of the self plants

# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

# This allows us to have the difference in height for each pair, which can be used to calculate the mean 
# difference in heights between paired plants, and the amount of variance (SD)
difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary
# just calculated the average difference in height between the groups of plants and the SD of the difference
# mean = 2.62, SD = 4.72

## 12.4.1 Standard error of the difference ----
# SE can be seen as the SD of the mean. SE describes the variability expected among sample means if the experiment was repeated
# SE = measure of the confidence have in an estimate of a true population mean

# We can calculate the standard error for our sample
difference_summary %>% 
  mutate(se= sd/sqrt(n))
# mean = 2.62, SD = 4.72, SE = 1.22

## 12.4.2 Activity 3: Communicate ----
# Task- how would you present a short sentence describing the average difference in height?
# The average difference in height was 2.62 ± 1.22 inches (mean ± SE)

# 12.5 Uncertainty ----
# SE is a measure of uncertainty, the larger the SE = more uncertainty. 
# Smaller SE = more confidence we can have that our difference in means is real

## 12.5.1 Normal Distribution ----
# Normal distribution = bell-shaped curve determined by mean & standard deviation

## 12.5.2 Confidence Intervals ----
# We can work out the 95% confidence interval range of our estimated mean as follows:
lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI  # 0.18
upperCI  # 5.06

# The maize plants that have been cross pollinated were taller on average than the self-pollinated plants,
# with a mean difference in height of 2.62 [0.18, 5.06] inches (mean [95% CI]).
