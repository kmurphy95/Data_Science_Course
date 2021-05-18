#Author: Kathy Murphy
#Date: November 6th, 2020
#Purpose: Tree-based Decision Methods

rm()

library(MASS)
library(tree)

set.seed(1)
N <- nrow(Boston)
train <- sample(1:N, N/2) #Create a training set out of half of the original data
test <- seq(1:N)[-train] #Create a test set from all data not included in train

#1 Fitting Regression Trees
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)
#4 variables used instead of 3?


#Plot regression tree
plot(tree.boston)
text(tree.boston) #add text/labels to tree plot

#Prune Tree
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")

#The unpruned tree gives the best results according to cross-validation

#Example of pruning with only 5 branches:
#prune.boston <- prune.tree(tree.boston, best=5)
#plot(prune.boston)
#text(prune.boston)

#Tree predictions
yhat.tree <- predict(tree.boston, Boston[test,])
boston.test <- Boston[test, "medv"]
plot(yhat.tree, boston.test)
abline(0,1)
mean((yhat.tree-boston.test)^2)

#Linear model predictions
lm.boston <- lm(medv~ ., Boston, subset=train)
yhat.lm <- predict(lm.boston, Boston[test,])
boston.test <- Boston[test, "medv"]
mean((yhat.lm-boston.test)^2)

#2 Random Forests & Bagging
library(randomForest)
set.seed(1)
P <- ncol(Boston)-1
bag.boston <-  randomForest(medv~., data=Boston, subset=train, mtry=P,
                            importance=TRUE)
bag.boston

yhat.bag <- predict(bag.boston, Boston[test,])
mean((yhat.bag-boston.test)^2)

#Change number of trees in forest
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=P, ntree=25)
yhat.bag <- predict(bag.boston, Boston[test,])
mean((yhat.bag-boston.test)^2)

rf.boston <- randomForest(medv~., data=Boston, subset=train, ry=6,
                          importance=TRUE)
yhat.rf <- predict(rf.boston, Boston[test,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)

varImpPlot(rf.boston)

#3 Boosting
library(gbm)
set.seed(1)

boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees
                    =5000, interaction.depth=4)
summary(boost.boston)

#Partial Dependence Plot for # rooms and socioeconomic status
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost <- predict(boost.boston, newdata=Boston[test,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees 
                    =5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, Boston[test,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
