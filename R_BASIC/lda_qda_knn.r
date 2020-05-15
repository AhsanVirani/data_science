
##################### LINEAR DISCRIMINANT ANALYSIS ###########################
# LDA by lda command. Same syntex just no family function
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset = train)
lda.fit
plot(lda.fit)
attributes(lda.fit)
lda.fit$xlevels

lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.pred$posterior
lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
# HOW MANY DOWN (POSTERIOR)
?sum
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

##################### QUADRATIC DISCRIMINANT ANALYSIS ##########################
### qda() function. IN MASS LIBRARY. Identical syntax to LDA

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset = train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
attributes(qda.class)
qda.class$class
?attributes
?predict
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

#################### K-Nearest-Neighbour (KNN) #############################
## Part of Class Library
## knn() function forms predictions using a single command
library(class)
?knn
?cbind
## cbind() funciton combines vectors or matrices into one data frame
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
# With k = 1
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# With k = 3
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)



################################## QUESTIONS
## Question 10 Solution
library(ISLR)
fix(Weekly)
dim(Weekly)

summary(Weekly)
pairs(Weekly)
?cor
str(Weekly)
corrMat <- cor(Weekly[,-9])
# Year and VOlume seems to have correlation

attach(Weekly)
?glm
log.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly,
               family = binomial)
summary(log.fit)
# Lag 2 is stat sig
?predict
log.prob <- predict(log.fit, data = Weekly, type = 'response')
log.pred <- rep("Down", 1089)
log.pred[log.prob > 0.5] = "Up"
table(log.pred, Direction)

train <- (Year<2009)
tail(train)
test <- Weekly[!train,]
log.fit1 <- glm(Direction ~ Lag2, data = Weekly,
               family = binomial, subset = train)
log.prob1 <- predict(log.fit1, test, type = 'response')
log.pred1 <- rep("Down", nrow(test))
log.pred1[log.prob1 > 0.5] = "Up"
table(log.pred1, Direction[!train])
mean(log.pred1 == Direction[!train])

library(MASS)
### FITTING LDA
lda.fit <- lda(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly)
lda.prob <-  predict(lda.fit, Weekly, type = 'response')
attributes(lda.prob)
lda.prob$class
table(lda.prob$class, Direction)

lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.prob <-  predict(lda.fit, test, type = 'response')
attributes(lda.prob)
lda.prob$class
table(lda.prob$class, Direction[!train])

## FITTING QDA
### FITTING LDA
qda.fit <- qda(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly)
qda.prob <-  predict(qda.fit, Weekly, type = 'response')
attributes(qda.prob)
qda.prob$class
table(qda.prob$class, Direction)

qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.prob <-  predict(qda.fit, test, type = 'response')
attributes(qda.prob)
qda.prob$class
table(qda.prob$class, Direction[!train])


### USING KNN 
Weekly <- na.exclude(Weekly)
train.rows <- (Year < 2009)
?knn
train <- Weekly[train.rows, -c(1,9)]
test <- Weekly[!train.rows, -c(1,9)]
train.Dir <- Direction[train.rows]
library(class)
knn.pred <- knn(train, test, train.Dir, k = 1)
table(knn.pred, Direction[!train.rows])

train <-as.data.frame(Weekly$Lag2[train.rows]) 
test <- as.data.frame(Weekly$Lag2[!train.rows])
train.Dir <- Direction[train.rows]
library(class)
set.seed(1)
knn.pred <- knn(train, test, train.Dir, k = 1)
table(knn.pred, Direction[!train.rows])
mean(knn.pred==Direction[!train.rows])



fix(Auto)
attach(Auto)
median(mpg)
mpg01 <- rep(0, nrow(Auto))
dim(Auto)
mpg01[mpg > median(mpg)] = 1
head(mpg01,15)

?cbind
Auto <- as.data.frame(cbind(Auto, mpg01))
cor(Auto[,-9])
pairs(Auto)

## Divide in training and test Data Set
train.years <- (year < 81)
train <- Auto[train.years,]
test <- Auto[!train.years,]
mpg01_test_lab <- test$mpg01[] 

## LDA
?lda
?predict
lda.fit <- lda(mpg01~cylinders + weight + displacement + horsepower, data = train)
lda.prob <- predict(lda.fit, test, type='repsonse')
table(lda.prob$class, mpg01_test_lab)
mean(lda.prob$class != mpg01_test_lab)

## QDA
qda.fit <- qda(mpg01~cylinders + weight + displacement + horsepower, data = train)
qda.prob <- predict(qda.fit, test, type='repsonse')
table(qda.prob$class, mpg01_test_lab)
mean(qda.prob$class != mpg01_test_lab)

## Log Reg
glm.fit <- glm(mpg01~cylinders + weight + displacement + horsepower, data = train,
               family = binomial)
glm.prob <- predict(glm.fit, test, type = 'response')               
glm.pred <- rep(0, nrow(test))
glm.pred[glm.prob > 0.5] = 1
table(glm.pred, mpg01_test_lab)
mean(glm.pred != mpg01_test_lab)

### KNN
library(class)
?knn
mpg01_train_lab <- mpg01[train.years]
train <- Auto[train.years, c(2,3,4,5)]
test <- Auto[!train.years,c(2,3,4,5)]
sed.seed(1)
knn.pred <- knn(train, test, mpg01_train_lab , k = 1)
table(knn.pred, mpg01_test_lab)
mean(knn.pred != mpg01_test_lab)
