#Author: Kathy Murphy
#Date: 11/24/2020
#Purpose: Homework 3: Decision Trees

set.seed(42)

library(tree)
library(ggplot2)
library(randomForest)
library(gbm)

load("class_survey.RData")
attach(survey)

survey <- na.omit(survey)
#1. Make a plot of the response 'exercise' that shows the distribution of
#observations. Also report the minimum, maximum, and average value in the 
#response. Briefly comment on what you observe.
ggplot(survey, aes(x=exercise)) +
  geom_bar(fill = "cornflowerblue") 
summary(exercise)
#Minimum = 0.0
#Maximum = 14.0
#Average = 4.085

#Most people in the class seem to exercise about 3 hours a week, with some 
#exercising less, and a larger amount exercising slightly more. Two people
#report to exercise significantly higher than the median.


#2. Split your data into a training set and test set, with 12 randomly selected 
#observations (about 1/3 of the data) in the test set.

N <- nrow(survey)
train <- sample(1:N, N/3)
test <- seq(1:N)[-train]

#3. Estimate three random forests with all other columns in the data as 
#predictors. (Hint: The '.' operator in a formula will refer to all other
#columns in a data frame that haven't been included in the model yet. 

#Estimate one random forest using all P predictors sampled for splitting at
#each node, one with P/3, and one with the square root of P.

P <- ncol(survey)-1
bag.surveyP <- randomForest(exercise~., data=survey, subset=train, mtry=P,
                            importance=TRUE)

rf.surveyP3 <- randomForest(exercise~., data=survey, subset=train, mtry=P/3,
                             importance=TRUE)

rf.surveyPsqrt <- randomForest(exercise~., data=survey, subset=train,
                                mtry=sqrt(P), importance=TRUE)

#4. Estimate the test MSE for each of the three models. Briefly comment on what
#you observe. 
survey.test <-survey[test,"exercise"]

yhat.bagP <- predict(bag.surveyP,survey[test,])
yhat.bagP
mean((yhat.bagP-survey.test)^2)

yhat.rfP3 <- predict(rf.surveyP3,survey[test,])
yhat.rfP3
mean((yhat.rfP3-survey.test)^2)

yhat.rfPsqrt <- predict(rf.surveyPsqrt,survey[test,])
yhat.rfPsqrt
mean((yhat.rfPsqrt-survey.test)^2)

#Most of the predictions fall in the 3.5-5.5 range for the bagging and P/3
#forest models, while the sqrt(P) forest predicts slightly less precise 
#exercise times (in the 3.4-5.1 range) that are closer to the average value. 

#Which MSE is the smallest? Why?
#The random forest using a random selection of P/3 predictors has the lowest
#MSE. 

importance(bag.surveyP)
importance(rf.surveyP3)
importance(rf.surveyPsqrt)

  
#5. Fit a boosted regression tree and estimate its test MSE. 
#Try to find parameter settings that will result in a test MSE that is 
#smaller than the smallest test MSE you have estimated above. (Note: Set 
#bag.fraction = 1' when using the gbm() function. 'bag.fraction' specifies
# the fraction of the training set observations randomly selected to propose 
#the next tree in the expansion. The default value is 0.5, 
#which is too small for the size of our data.)
survey.test <- survey[test,"exercise"]

boost.survey <- gbm(exercise~social_media+games+facebook, 
                    data=survey[train,], distribution = "gaussian",n.trees=10000, 
                    bag.fraction = 1.8, interaction.depth = 4, verbose=F)
summary(boost.survey)

yhat.boost <- predict(boost.survey, newdata = survey[test,], n.trees = 10000)
mean((yhat.boost-survey.test)^2)

