# Install Packages ----
library(tidyverse)

# 14.2 Student's t-test -----
# This uses the t-distribution, small sample size version of the normal distribution, where tails are fatter if df are small
# 2 basic types of t-test

# One Sample t-test: takes the mean of a sample & compares it with the null hypothesis of 0
# Two Sample t-test: compares the difference between the means of 2 samples against a null hypothesis if no difference between the means of the two populations.
# General equation for calculating t is: t= difference/SE
# Calculation of t-value = counting standard errors (est. of diff is about 2x large as the SE at 95% CI).
# When sample sizes are large the t-distribution is roughly = to normal (z) distribution - less robust at small sample sizes.

# The below code allows for the comparison of distributions
# The Base R Version 
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

# The Tidyverse Version
x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

# There are 2 VALUES THAT HAVE TO BE CONSIDERED WHEN DISCUSSING T.
# 'critical t' - the value which must be exceeded for the test to be significant- defined by df
# 'observed value of t'- value returned by statistical test, calculated by difference/SE.
# When 'observed t' > 'critical t' the result can be declared significantly different @ threshold for alpha.

# The values for critical t at each degreee of freedom up to 30 are presented below.
df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

# Used mixture of base R summary() function & broom::tidy() tibbles.
# Using either method can see that they include t-tests for the coefficient. 
lsmodel1 <- lm(height ~ type, data = darwin)
lsmodel1

# 14.2 Task - Run a Function to generate a summary of your model ----
# This can once again be done using either Base R or the Tidyverse Broom Package
summary(lsmodel1)   # Base R
broom::tidy(lsmodel1)  # Tidyverse Broom Package

# linear model summary automatically applies test to EVERY ROW- sometimes these are important tests defined by hypothesis.
# 2nd row of table tests null hypothesis by comparing the avg. observed difference in plant heights between cross & self-pollinated plants
  # it then calculates the average diff. (estimate), amount of uncertainty (std. error) & calculates an 'observed t value'
  # this determines the probability of observing an effect of at least this size (@ this sample size) if the null hypothesis is true.

# 1st row also performs t-test, tests null hypothesis that intercept (mean height of cross-pollinated plants) = 0. Not useful.

# Observed difference in plants heights was 2.62 inches ± 1.07, produces observed value of t as:
tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]
# t = -2.437113
# This could be the same as the value for t in our model summary (called statistic in the tidyverse= -2.44)

# 14.3 Paired t -----
# We have to add the factor for pairs to our linear model formula in order to generate a paired t-test.
# Once again this can be done using either Base R or tidyverse.
lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)  # Base R
# Note that I have made pair a factor - pair 2 is not greater than pair 1 - so it does not make sense to treat these as number values.
 
darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()           # Tidyverse

# The table of coefficients produced now looks more complicated - THE INTERCEPT IS NOW THE HEIGHT OF THE CROSSED PLANT FROM PAIR 1
# 2nd row now compares the mean heights of crossed & self plants when they are in the same pair.
# rows 3-16 compare the avg. difference of each pair (crossed & self combined) against pair 1.
# we only care about the difference in cross & self pollinated plant heights.

# The below code will generate confidence intervals for the paired t-test.
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows
# the estimate of the mean difference is identical, but 95% CI are now slightly different. 
# We have increased our level of uncertainty by including the pair parameter.

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

# 14.6 Repeatability -----
# Not possible to know from a single experiment whether made type 1 or type 2 errors.
 # Over time when experiments are repeated literature builds allowing evidence to be built. 

# Example below we imagined that the Darwin experiment had been repeated 20 times. 
# Assume we 'know' the true mean difference of crossed & fertilised plants & the standard deviation of the 'population'.
# Then creates 20 new sampling experiments & calculates the estimated mean difference for each experiment.
set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments


# 14.7 Activity 1: Experimental Repeatability ----
# Task- using this newly generated data, how many experiment found a signficant difference and how many did not?
        # What would you conclude from this?

y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())
# 6 of the experiments were non-significant, whilst 14 were.
# nearly a third did not find a statistically significant difference --> look at the estimates & calculate the confidence intervals to ensure results are consistent.

## Task- using this newly generated data, compare the estimates & confidence intervals.
         # What could you conclude from this?
y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

# The results are not really inconsistent, -ve effects of inbreeding depression are clear in all experiments
