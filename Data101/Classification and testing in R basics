library(MASS)
library(ISLR)
fix(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)
str(Smarket)
corr_mat <- cor(Smarket[,-9])
## Corr matrix shows correlation between Year and Volume
attach(Smarket)
plot(Volume)
## glm() Function fits generalized linear models, a class of models that includes logistic regression
# Must pass arguement family=binomial
glm_fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data = Smarket, family = binomial)
?glm
summary(glm_fit)
coef(glm_fit)
glm_probs = predict(glm_fit, type="response")
glm_probs[1:10]
contrasts(Direction)
?rep

## Our predictions
glm_pred = rep("Down", 1250)
glm_pred[glm_probs > 0.5] = "Up"

# Confusion matrix using table command
table(glm_pred, Direction)
?table
mean(glm_pred==Direction)

## Problem? Trained and tested on same data set
# Training Error rate of 100%-52.2% = 47.8%
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 = Smarket$Direction[!train]
contrasts(Direction.2005)
?subset

glm_fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family = binomial, subset = train)
glm_probs=predict(glm_fit, Smarket.2005, type = "response")
?predict
glm_pred=rep("Down", 252)
glm_pred[glm_probs>0.5]="Up"
table(glm_pred, Direction.2005)
mean(glm_pred==Direction.2005)
## Test Error Rate
mean(glm_pred!=Direction.2005)

## Lag3-5 very large p-value
glm_fit1 = glm(Direction~Lag1+Lag2,data=Smarket, family = binomial,subset = train)
summary(glm_fit1)
glm_probs1 = predict(glm_fit1, Smarket.2005,type="response")
glm_pred1 = rep("Down", 252) 
glm_pred1[glm_probs1>0.5] = "Up"
table(glm_pred1, Direction.2005)
mean(glm_pred1 == Direction.2005)

predict(glm_fit1, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),
                                        type="response")
