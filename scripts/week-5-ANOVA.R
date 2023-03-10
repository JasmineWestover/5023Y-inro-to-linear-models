# Install Packages ----
library(tidyverse)
library(here)

# Maize Data Imported ----
darwin <- read_csv(here("data", "darwin.csv"))

# 16.1 Analysis of Variance (ANOVA) -----
# In previous weeks, we used linear models for analysis between two 'categorical' explanatory variables e.g. t-tests, and regression with two continuous variables.
# As models become more elaborate, the number of pairwise t-tests increase, as does our risk of false positives (Type 1 errors).
# ANOVA = Difference between the different means before diving into multiple comparisons. 
# ANOVA is just another type of linear model, but it tends to be associated with categorical data.

# 16.2 Maize Data ----
# Our simple linear model for the maize data was:
lsmodel1 <- lm(height ~ type, data = darwin) # analyses the differences in average plant height, can use the lm() function

# One-way ANOVA = Fitting a linear model, with one explanatory variable. 
# ANOVA = Quantify overall variability in the data set & divide it into variability between & within the groups. This is called 'SIGNAL-TO-NOISE' RATIO
# Method of fitting a model = ordinary least squares.
# Model splitting overall variability (SST) into signal (SSR) and noise (SSE): SST = SSR + SSE

## 16.2.1 The ANOVA Table ----
# If we want to get the ANOVA Table for a linear model we can use the anova() function:
anova(lsmodel1) 
# F-Value =  5.9395 (5.94), P-Value = 0.02141 & df = 1 & 28

# Compare results of the ANOVA table to the summary function
summary(lsmodel1)
# F-Value = 5.94 , P-Value = 0.02141 & df = 1 & 28

# The outputs of the two above functions are the same - ANOVA table has less info, as has no info about the BIOLOGY (estimates in difference, or uncertainty i.e. SE).
# Help simplify effects when there are more than two levels.

### The AVOVA Table has 6 columns & 2 rows ----
    # 1st Column = information source- signal/regression (first row) and error/noise (second row)
    # 2nd Column = degrees of freedom- 1st row is no. of predictors (treatments) (k-1, including intercept) & 2nd row is residual degrees of freedom (N-k)
    # 3rd Column = Sum of squares in the 1st row this is SSR, & 2nd row SSE (SST = SSR + SSE)- MORE ABOUT THIS IN COURSEBOOK & Lecture
    # 4th Column = mean sum of Squares MSR = SSR/(k-1) & MSE = SSE/(N-k). These are the avg. amounts of variability per treatment level.
    # 5th Column = F Statistic (signal-to-noise ratio). Calculated by dividing treatment variance by the residual error variance.       F= (SSR/(k-1))/(SSE/(N-k)) = MSR/MSE
      # In example, F-Value = 5.9, the estimated signal is nearly 6x larger than estimated noise. 
      # Use F-Value with sample size & treatment levels to calculate probability of observing this ratio of signal to noise if H0 that no effect of treatment is true. 
    # Probability value can be looked up in AVOVA table- calculated from an F Distribution. 
      # F-test takes sample size into account & probability assigned takes degrees of freedom of both the signal & noise into account. 
      # Able to see this is we the R Function pf)(), which should recreate the exact p-value seen in ANOVA Table:

pf(5.9395, 1, 28, lower.tail=FALSE)
# pf = 0.02141466, which was the same from the ANOVA table. 
# The 1st 3 arguments = F-Value, degrees of freedom for signal and noise. The last argument is to set this as a two directional test.

# In order to interpret the p-values need to know:
  # What test they came from, what the observed value of the test was & the degrees of freedom.
  # If these aren't provided these are called naked P-values.

# Good & conventional way to report result as an ANOVA would be (example from coursebook):
# The height of the cross-pollinated plants was significantly taller than the height of the self-pollinated plants (F1,28 = 5.9, P = 0.02).
    # This emphasises the statistical tests, but not the underlying biology- doesn't tell us anything about the heights of the plants, the estimated difference between them, or any measure of the uncertainty around this. 

# Better example to report the data shown below (another example from coursebook):
  # The self pollinated maize plants measured an average of 17.6 [16-19.1] (mean[95% CI]) inches high, while the cross-pollinated plants had a mean height of 20.2 [18.6-21.7] inches - a difference of 2.6 [-0.4-4.8] inches (one-way ANOVA: F1,28 = 5.9, P = 0.02).

# 16.3 Two-way ANOVA ----
# Includes TWO explanatory variables- treatments of interest, but for this example we stick with including pair variable.

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin) # new object created 

# can look at this table using the anova() function again
anova(lsmodel2)
