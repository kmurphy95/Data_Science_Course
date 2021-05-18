# Author: Kathy Murphy
# Date: September 18, 2020
# Purpose: Linear Regression in R

#Load the data on MA, and Boston
library(MASS)
library(ggplot2)

View(Boston)
names(Boston)

#Question: What happens when we apply summary to Boston?
summary(Boston)
#Answer: A summary of each of the columns in Boston is provided, however the 
#        summary treats categorical variables as continuous, and so some of the 
#        statistics that it provides are not meaningful. 

#Loads the dataset into the workspace
attach(Boston)

#Fit a linear model to lstat vs medv
lm.fit <- lm(medv ~ lstat)

#Question: What  is  the  association  between  median  house  value  and  
#          percent  of  households  with  lowsocioeconomic  status?  

#Answer: A change in the percent of neighborhoods that are of low socioeconomic
#        status by 1% is associated with an average decrease in median home
#        value in those neighborhoods by $950.05.

#Estimate 95% confidence interval
confint(lm.fit)

summary(lm.fit)
#Question: How well would you say the model fits with the data?

#Answer: There is a very, very slight decrease in median home value as 
#        lower socioeconomic status is observed.

#lstat = 5 prediction
-.5441*5 + 34.55384

#lstat = 10 prediction
-.5441*10 + 34.55384

#lstat = 15 prediction
-.5441*15 + 34.55384

predict(lm.fit, data.frame(lstat = c(5, 10 , 15)), interval = "confidence")
#Question: Compare the results to the prediction you have done "by hand". 
#          Do you get the same or dif-ferent results?

#Answer: The answers are significantly different. 

#Create a scatterplot of the data
#lstat vs medv, with ggplot
ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method='lm')


#Exercises 

Auto <- read.table("http://faculty.marshall.usc.edu/gareth-james/ISL/Auto.data",
                   header=TRUE, na.strings="?")

#Auto is new workspace
attach(Auto)

lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)



#Question 1: Briefly describe the association between mpg and hp.

#Answer: A change in horsepower by one unit is associated with an average 
#        decrease in miles-per-gallon of 0.157845.


#Question 2: Is the estimated relationship statistically significant?

#Answer: Yes, the p value is extremely low, so the null hypothesis can be
#        ruled out. 

#Question 3: Briefly comment of the size of R^2. How much of a variance in the 
#            response is explained by the model? 

#Answer: R^2 shows that there is a slight relationship between horsepower and
#        mpg. The RSE is 4.906.   On average, the observed response will be off 
#        by 4.906 mpg compared to the estimate. 

#Question 4: The average horsepower of a typical vehicle today is between 120
#            (a small sedan) and 300 horsepower (a typical pickup truck). Use
#            your model to calculate expected miles per gallon for a vehicle 
#            with 120, 200 and 300 horsepower. Briefly comment on what you 
#            observe.

#Horsepower = 120
120*-.157845 + 39.935861
#Horsepower = 200
200*-.157845 + 39.935861
#Horsepower = 300
300*-.157845 + 39.935861

#Use R to predict
predict(lm.fit, data.frame(horsepower = c(120, 200, 300)), 
        interval = "confidence")
confint(lm.fit)

#Answer: Making the predictions by hand provides bad estimates. R's estimation
#        is much better, but struggles to predict a reasonable value when 
#        provided a predictor with no associated responses in the data.

#Question 5: Use ggplot to produce a scatterplot of the response and the 
#            predictor that includes the regression line you have estimated.

#Answer: See plot.
ggplot(Auto, aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_smooth(method='lm')
