#Author: Kathy Murphy
#Date: October 9th, 2020
#Purpose: Practice using resampling methods


rm()

#Load ISLR library and attach default data set
library(ISLR)
attach(Default)
summary(Default)

set.seed(42)



#VALIDATION SET APPROACH

#Randomly split the data into a training and test set
N <- nrow(Default)
train <- sample(N, N/2)

#Estimate logistic regression using balance and income on training set
glm.fit <- glm(default ~ balance + income, family=binomial, subset=train)

#Estimate predicted probabilities for default for each observation
#FOR THE DATA IN THE TEST SET
pred.probs <- predict(glm.fit, Default[-train, ], type="response")


pred.default <- rep("No", N/2)
pred.default[pred.probs > 0.5] <- Yes

#Calculate the MISCLASSIFICATION RATE
error.rate <- mean(pred.default != default[-train])


#k-Fold Cross Validation
library(boot)

#Define "cost" function for k-Fold validation with a threshold of 0.5
defaultcost <- function(r, pi = 0){
  mean(abs(r-pi) > 0.9)
}

#Create generalized linear model for probability of default based on balance
#and income

##################
glm.fit <- glm(default ~ balance + income, family = binomial)
cv.error.10 <- cv.glm(Default, glm.fit, cost=defaultcost, K=5)
cv.error.10$delta


