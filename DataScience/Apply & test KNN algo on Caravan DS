## Applying the knn approach to Caravan data set
library(ISLR)
fix(Caravan)
dim(Caravan)
attach(Caravan)
summary(Purchase)
mean(Purchase)
348/(5474+348)

## Standardize data so that all variables are given mean of 0 and var of 1
# scale() function
str(Caravan)
# purchase is a factor, hence must not be used for scaling
#col num of purchase
data.frame(colnames(Caravan))
standardized <- scale(Caravan[,-86])
?scale
var(Caravan[,1])
var(Caravan[,2])
var(standardized[,1])
var(standardized[,2])

test <- 1:1000
train.X <- standardized[-test,]
test.X <- standardized[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
library(class)
?knn
knn.pred <- knn(train=train.X, test = test.X, cl=train.Y, k =1)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

## With k = 3 
knn.pred <- knn(train=train.X, test = test.X, cl=train.Y, k =3)
table(knn.pred, test.Y)

## With k = 5
knn.pred <- knn(train=train.X, test = test.X, cl=train.Y, k =5)
table(knn.pred, test.Y)

## Fitting logistic regression as comparison purpose
glm.fit = glm(Purchase~., data=Caravan, family = binomial, subset = -test)
glm.prob = predict(glm.fit, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.prob > 0.5] = "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.prob > 0.25] = "Yes"
table(glm.pred, test.Y)
