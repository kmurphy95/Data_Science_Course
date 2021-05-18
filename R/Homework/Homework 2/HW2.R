#Author: Kathy Murphy
#Date: November 8th, 2020
#Purpose: Classification Homework

rm()

library(ggplot2)
library(boot)

load("class_survey.RData")
survey <- na.omit(survey)
survey$facebook <- as.factor(survey$facebook)
survey$football <- as.factor(survey$football)
survey$us <- as.factor(survey$us)

#Part 1
#Pick any binary qualitative variable as a response: 'facebook'
summary(survey$facebook)

ggplot(survey, aes(x=facebook)) +
  geom_bar(fill = "cornflowerblue") 

#Briefly comment on what you observe: Over five times as many students responded
#as not being a facebook user compared to those who did report being a facebook
#user.

#Part 2
#Estimate a logistic regression model that includes at least 4 predictors
attach(survey)

glm.fit <- glm(facebook ~ semesters_college + exercise +
                 roommates + us, family = binomial)
summary(glm.fit)
#Briefly comment on the results of the model:  the number of semesters that
#the respondent has spent in college and whether they are from the us 
#are significant in the model. 

#An increase in semesters spent in college and in exercise time per week has
#a positive effect on the respondent's chance of being on facebook. Being from
#the United states and having more roommates has a negative effect on the chance

#Part 3
#Use your model to make a prediction by varying one of the predictors and 
#holding all others constant
pred.semesters <- data.frame(semesters_college=c(2,6,10), exercise=3.25, 
                             roommates=2, us="Yes")
predict(glm.fit, pred.semesters, type="response")

#Part 4
#Use 5-fold cross validation to estimate the missclassification error with a 
#threshold of your choice.

#Cost function with threshold .2
newcost <- function(r, pi = 0){
  mean(abs(r-pi) > 0.2)
}

cv.error.5 <- cv.glm(survey, glm.fit, newcost, K=5)
cv.error.5$delta

#Part 5
#Use LOOCV to estimate the missclassification error with the same threshold. 
#Briefly comment on what you observe. 
loocv.error <- cv.glm(survey, glm.fit, newcost, K = nrow(survey))
loocv.error$delta


#Do 5-fold CV and LOOCV give you the same results? What is the size of the 
#missclassification error? Would you say this is an acceptable error rate?

#LOOCV provides a different error than 5-fold, with 5-fold's rate 
#being about 0.1 higher than LOOCVs. Both of which have missclassification rates
#in excess of 15% which is too high to accept in either case.
