#Author: Kathy Murphy
#Date: October 23rd, 2020
#Purpose: Non-linear models lab 7

rm()

library(ISLR)
library(splines)
attach(Wage)

#PART 1: Polynomial Linear Regression 
fit <- lm(wage ~ poly(age,4))

age.grid <- seq(from = min(age), to = max(age))
pred <- predict(fit, newdata = list(age = age.grid), se=TRUE)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)

plot(age, wage, cex=.5, col="darkgray", main="Degree-4Polynomial")
lines(age.grid, pred$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

#Exercise 1: Write a loop to estimate the {1-4} degree polynomial linear 
#regressions. Plot all four fits and confidence bands in one plot.

I <- c(1:4)
plot(age, wage, cex=.5, col="darkgray", main="Degree-1:4 Polynomial")
colors <- c("red", "orange", "purple", "cyan")

for (val in I)
{
  fit <- lm(wage ~ poly(age,val))
  
  age.grid <- seq(from = min(age), to = max(age))
  pred <- predict(fit, newdata = list(age = age.grid), se=TRUE)
  se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
  
  lines(age.grid, pred$fit, lwd=2, col=colors[val])
  matlines(age.grid, se.bands, lwd=1, col=colors[val], lty=3)
  
}



#PART 2 Polynomial Logistic Regression

fit <- glm(I(wage > 250) ~ poly(age, 4), family=binomial)

pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
pfit <- exp(pred$fit) / (1+ exp(pred$fit))
se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

plot(age, I(wage>250), type="n", ylim=c(0,.2))
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)
rug(jitter(age[wage<=250]), side = 1, ticksize = 0.02)
rug(jitter(age[wage>250]), side = 3, ticksize = 0.02)

#Exercise 2: Modify your code from Exercise 1 to plot the {1-4}-degree
#polynomial logistic regression in one plot

I <- c(1:4)
plot(age, I(wage>250), type="n", ylim=c(0,.2))
rug(jitter(age[wage<=250]), side = 1, ticksize = 0.02)
rug(jitter(age[wage>250]), side = 3, ticksize = 0.02)

for (val in I)
{
  fit <- glm(I(wage > 250) ~ poly(age, val), family=binomial)
  pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
  pfit <- exp(pred$fit) / (1+ exp(pred$fit))
  se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
  se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
  
  lines(age.grid, pred$fit, lwd=2, col=colors[val])
  matlines(age.grid, se.bands, lwd=1, col=colors[val], lty=3)
  lines(age.grid, pfit, lwd=2, col=colors[val])
  matlines(age.grid, se.bands, lwd=1, col=colors[val], lty=3)
  
}


#PART 3: Step Functions

fit <- lm(wage ~ cut(age, 4))

#PART 4: Splines

knot.position <- c(25, 40, 60)
fit <- lm(wage ~ bs(age, knots=knot.position), data=Wage)

pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, cex=.5, col="darkgray")
lines(age.grid, pred$fit, lwd=2, col="red")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col="red")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col="red")
abline(v = knot.position, lty="dashed")

#Exercise 3: Estimate and plot a cubic regression spline with knots
#at the 25th, 50th, and 75th percentiles of age
percentiles <- quantile(age, c(.25,.50,.75))

knot.position <- percentiles
fit <- lm(wage ~ bs(age, knots=knot.position))

pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, cex=.5, col="darkgray")
lines(age.grid, pred$fit, lwd=2, col="red")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col="red")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col="red")
abline(v = knot.position, lty="dashed")

#Exercise 4: Add the predictions from a natural cubic regression spline to the
#plot from Exercise 3. (Hint: Natural splines can be estimated with the ns() 
#function.)

fit <- lm(wage ~ ns(age, knots=knot.position))
pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
lines(age.grid, pred$fit, lwd=2, col="cyan")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col="cyan")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col="cyan")
abline(v = knot.position, lty="dashed")


#PART 5: Smoothing Splines
fit <- smooth.spline(age, wage, df=16)


fit2 <- smooth.spline(age, wage, cv=TRUE)


plot(age, wage, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF (LOOCV)"), col=c("red", "blue"),
       lty=1, lwd=2, cex=.8)

#PART 6: Generalized Additive Models (GAM)

gam1 <- lm(wage ~ ns(year, 4) + ns(age,5) + education)

gam2 <- gam(wage ~ s(year, 4) + s(age, 5) + education)

par(mfrow = c(1,3))
plot(gam2, se=TRUE, col="blue")
