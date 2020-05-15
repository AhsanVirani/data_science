## MASS and ISLR libraries contain Statistics functions to perform LR etc
library(MASS)
library(ISLR)
names(Boston)
?Boston
#Fits a linear model
lm.fit = lm(medv~lstat, data = Boston)
?lm
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
?confint

# Predictions
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval = "confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval = "prediction")
plot(Boston$lstat, Boston$medv)
abline(lm.fit)
# Different Width
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="Red")
plot(Boston$lstat, Boston$medv, col="Red")
# pch function can change data points shape
plot(Boston$lstat, Boston$medv, pch=20)
plot(Boston$lstat, Boston$medv, pch="+")

plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# Leverage predictor
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Regression
lm.fit = lm(medv~lstat+age, data = Boston)
summary(lm.fit)
lm.fit = lm(medv~., data = Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
?summary.lm

attach(Boston)

# All variables in regression except age
lm.fit = lm(medv~.-age,data = Boston)
summary(lm.fit)
lm.fit = update(lm.fit,~.-age)

# Interaction term
lm.fit = lm(medv~lstat*age,data = Boston)
summary(lm.fit)
# Raising to power
lm.fit2 = lm(medv~lstat+I(lstat^2),data = Boston)
summary(lm.fit2)
# anova() function to further quantify the extent to which the quadratic fit is superior to a linear fit
lm.fit = lm(medv~lstat)
anova(lm.fit,lm.fit2)
plot(lm.fit)

#polynomial fit
lm.fitp = lm(medv~poly(lstat,5))
summary(lm.fitp)

# Qualitative Predictors
attach(Carseats)
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
contrasts(ShelveLoc)

# Writing Function to call our MASS and ISLR libraries
LoadLibraries=function(){
  library(MASS)
  library(ISLR)
  print("The Libraries Have Been Loaded")
}
LoadLibraries()

################################################### ANSWERS TO QUESTIONS ##########################################################
###################################### Question 8
library(ISLR)
library(MASS)
Auto = read.csv("Auto.csv")
str(Auto)
attach(Auto)
# Fitting linear Model
Auto$horsepower = as.integer(horsepower)
lm.fit = lm(mpg~horsepower, data = Auto)
?lm
summary(lm.fit)
# CI AND PI at 95% 
confint(lm.fit, level = 0.95)
predict(lm.fit, data.frame(horsepower=c(100, 110, 120)),interval='prediction',level=0.95)
?predict
# Plotting 
plot(horsepower, mpg)
?abline
abline(lm.fit, col="blue")
plot(lm.fit)

######################################## Question 9
pairs(Auto) # cant see anythinhg clearly here
str(Auto)
Auto$name = as.factor(name)
# Computing correlations of variables except names (a factor)
?cor
corr.mat = cor(Auto[,-9])
corr.mat
# Multiple Regression fit
Mul.Reg = lm(mpg~.-name, data = Auto)
summary(Mul.Reg)
# plot of regression
# Setting margins and space to plot 4 images side by side
par(mfrow=c(2,2))
par(mar = c(1,1,1,1))
plot(Mul.Reg)
# defaulting image space
par(mfrow=c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))

# Adding an interaction term in the analysis
Mul.Reg1 = lm(mpg~.-name + horsepower:weight, data = Auto)
summary(Mul.Reg1)
# Checking whether its a better model
?anova
anova(Mul.Reg, Mul.Reg1)

######################### QUESTION 10
fix(Carseats)

## Reg
attach(Carseats)
lm.fit = lm(Sales ~ Population+Urban+US, data = Carseats)
summary(lm.fit)
lm.fit1 = lm(Sales ~ US, data = Carseats)
summary(lm.fit1)
anova(lm.fit1, lm.fit)
?anova

# CI
confint(lm.fit1)
# High Leverage Points
plot(lm.fit1)

############################ QUESTION 11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
# Reg without an intercept
lm.fit = lm(y~x+0)
summary(lm.fit)
lm.fit1 = lm(x~y+0)
summary(lm.fit1)

############################ QUESTION 13
set.seed(1)
X = rnorm(100)
?rnorm
eps = rnorm(100, mean=0, sd=sqrt(0.25))
y_pure = -1+0.5*X
length(y_pure)
y = y_pure + eps
plot(x, y)
fit = lm(y~x)
summary(fit)
?abline
abline(fit, col='Red')
abline( a=-1, b=1/2, col='green' ) 
legend( -3, 1, c("estimated","truth"), col=c("black","green"), lty=c(1,1) )
?legend

# Quad fit
fit1 = lm(y~poly(X, degree = 2))
summary(fit1)
anova(fit1, fit)

######################## Question 14
# Collinearity Problem
set.seed(1)
?runif
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
corrXY <- cor(x1,x2)
fit = lm(y~x1+x2)
summary(fit)
fit1 = lm(y~x1)
fit2 = lm(y~x2)
x1 = c(x1,0.1)
x2 = c(x2,0.8)
y = c(y,6)
plot(lm(y~x1+x2))
plot(lm(y~x1))
plot(lm(y~x2))
