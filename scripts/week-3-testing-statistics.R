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
