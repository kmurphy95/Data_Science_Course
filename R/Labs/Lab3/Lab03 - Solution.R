rm(list = ls())

# Author: Alex Herzog
# Date: Fall 2020
# Purpose: Demonstrate linear regression in R


# Exercise
# --------
# This exercise involves the use of simple linear regression on the Auto data set. 
# This data set is included in library 'ISLR', which you have to load at the beginning of your script.

# Perform a linear regression with mpg (miles per gallon) as the response and horsepower (engine horsepower) as the predictor. 
# Use the summary() function to print the results. 

library(ISLR)
lm.fit <-  lm(mpg ~ horsepower, data=Auto)
summary(lm.fit)


# 1. Briefly describe the association between miles per gallon and horsepower. 
# - A change by one horsepower is associated with an average change in miles per gallon by -0.16
# - An increase by one horsepower is associated with an average decrease in miles per gallon by 0.16

# 2. Is the estimated relationship statistically significant? (Hint: Look at the size of the p-value.)
# - The p-value is very small (<2e-16 ***), so yes, the estimated relationship statistically significant

# 3. Briefly comment on the size of the R^2 value. How much of the variance in the response is explained by the model?
# - The R-square value is 0.6059, so about 61% of the variance in the response is explained by the model. 

# 4. The average horsepower of a typical vehicle today is between 120 (a small sedan) and 300 horsepower (a typical pickup truck). Use your model to calculate expected miles per gallon for a vehicle with 120, 200 and 300 horsepower. Briefly comment on what you observe.
predict(lm.fit, data.frame(horsepower = c(120, 200, 300)))

# - The predicted values are 20.994493, 8.366914, and -7.417559. For 300 horsepower, the prediction is negative. 
# - The maximum value of horsepower in our data is 230. A prediction for 300 is therefore out of sample, and because the linear model extends to infinity in both directions, we observe a negative value.

# 5. Use ggplot to produce a scatter plot of the response and the predictor that includes the regression line you have estimated.
ggplot(Auto, aes(x = horsepower, y = mpg)) + 
  geom_point() + 
  geom_smooth(method='lm')


