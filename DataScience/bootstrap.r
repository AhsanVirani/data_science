############################# BOOTSTRAP ##################################
library(boot)
library(ISLR)
fix(Portfolio)

alpha.fn = function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace = T))
?sample
?boot
boot(Portfolio, alpha.fn, R = 1000)

fix(Auto)
attach(Auto)
boot.fn = function(data, index) {
  return( coef(lm(mpg~horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

# Now estimating using bootstrap
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot.fn, R=1000)
summary(lm(mpg~horsepower, data = Auto))$coef
# Bootstrap is giving much more accurate estimation of the SE of B0 and B1 than linear model (summary func)
boot.fn = function(data, index) {
  return( coef(lm(mpg~horsepower+I(horsepower^2), data = data, subset = index)))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data = Auto))$coef
