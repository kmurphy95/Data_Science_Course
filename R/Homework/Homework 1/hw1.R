#Author: Kathy Murphy
#Date:   October 21st, 2020
#Purpose: Homework 1: Linear Regression 

rm()
library(ggplot2)


#Part 1, load and clean data
load("class_survey.RData")
dim(survey)
survey <- na.omit(survey)

#Part 2: ggplot Histogram
ggplot(data = survey) +
  geom_histogram(mapping = aes(x=games, color="red"), bins = 8) +
  ggtitle("Hours of Video Games per Week")

#Part 3: Linear Regression Model
lm.fit <- lm(survey$games ~ survey$us + survey$os + survey$social_media + 
               survey$exercise+ survey$facebook + survey$movies + 
               survey$roommates + survey$pineapple + survey$death_valley + 
               survey$states_visited +survey$sweet_tea + 
               survey$semesters_clemson + survey$semesters_college +
               survey$country + survey$football +survey$siblings + 
               survey$distance_home)

summary(lm.fit)

#Part 4: Explain significant predictor (Number of States Visited)

#For each additional state a person has visited, the average number of hours 
#spent playing video games decreases by 0.394, holding all other predictors 
#constant.


#Part 5: Briefly comment on how well the model fits the data

#The p-value of our model is greater than 0.05 and thus it may be statistically
#insignificant.

#The RSE is 7.812, which means that on average, responses will differ from the 
#true regression line by 7.812 hours. The model explains 21.5% of the variation
#in the number of hours played.

