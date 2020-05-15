library(ISLR)
fix(Default)
attach(Default)
# Logistic Reg
set.seed(1)
log.fit <- glm(default~income+balance, family = binomial)
?glm
?sample

estimate_pred <- function() {
  train <- sample(nrow(Default), 8000)
  log.fit <- glm(default~income+balance, data = Default, 
                 family = binomial, subset = train)
  
  log.prob <- predict(log.fit, Default[-train,], type = 'response')
  glm.pred <- rep("No", nrow(Default[-train,]))
  glm.pred[log.prob > 0.5] = "Yes"
  tail(glm.pred)
  return(mean(glm.pred!=Default[-train, "default"]))
}
estimate_pred()

estimate_pred_stu <- function() {
  train <- sample(nrow(Default), 8000)
  log.fit <- glm(default~income+balance+student, data = Default, 
                 family = binomial, subset = train)
  
  log.prob <- predict(log.fit, Default[-train,], type = 'response')
  glm.pred <- rep("No", nrow(Default[-train,]))
  glm.pred[log.prob > 0.5] = "Yes"
  tail(glm.pred)
  return(mean(glm.pred!=Default[-train, "default"]))
}
estimate_pred_stu()


set.seed(1)
log.fit$coefficients
summary(glm(default~income+balance, data = Default, 
            family = binomial, subset = train))
boot.fn <- function(data, index) { return(coef(glm(default~income+balance, data=data,
                                               family=binomial, subset=index)))}
library(boot)
?boot
boot(Default, boot.fn, 50)


library(ISLR)
fix(Default)
attach(Default)

##### Q8
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x, y)

library(boot)
data = data.frame(x,y)
?data.frame
rm(data)
glm.fit = glm(y ~ poly(x, 4))
?cv.glm
cv.glm(data, glm.fit)$delta

###### Q9
library(MASS)
fix(Boston)
attach(Boston)
medv_mean = mean(medv)
medv_sd = sd(medv)
medv_se <- medv_sd/sqrt(nrow(Boston))

library(boot)
?boot
boot.fn = function(data, index) return(mean(data$medv[index]))
bstrap = boot(Boston, boot.fn, 100)
bstrap
c(bstrap$t0-2*0.402, bstrap$t0+2*0.402)
t.test(Boston$medv)

# e
medv_med <- median(medv)
boot.fn <- function(data, index) { return(median(data$medv[index]))}
bstrap = boot(Boston, boot.fn, 1000)
bstrap
?quantile()
medv_tenth <- quantile(medv, 0.1)
medv_tenth
boot.fn <- function(data, index) { return(quantile(data$medv[index], 0.1))}
bstrap = boot(Boston, boot.fn, 1000)
bstrap
