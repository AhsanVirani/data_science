################# Trees Questions ##############################
#Q7
library(MASS)
fix(Boston)
library(randomForest)
library(tree)
attach(Boston)
?tree
?randomForest
set.seed(1)

# Create train and test data
train = sample(nrow(Boston), nrow(Boston)/2)
x.test = Boston[-train, -14]
test_response = Boston[-train, 14]
x.train = Boston[train, -14]
train_response = Boston[train, 14]

# number of independent variables
p = dim(Boston)[2] - 1
p_half = p/2
p_root = sqrt(p)

rf.boston.p <- randomForest(x = x.train, y = train_response,
                            xtest = x.test, ytest = test_response,
                            mtry = p, ntree = 500)
rf.boston.phalf <- randomForest(x = x.train, y = train_response,
                            xtest = x.test, ytest = test_response,
                            mtry = p_half, ntree = 500)
rf.boston.proot <- randomForest(x = x.train, y = train_response,
                            xtest = x.test, ytest = test_response,
                            mtry = p_root, ntree = 500)
plot(1:500, rf.boston.p$test$mse, col = "green", type = "l",
     xlab = "Number of trees", ylab = "Test MSE", ylim = c(15, 40))
lines(1:500, rf.boston.phalf$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.proot$test$mse, col = "blue", type = "l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
       cex = 1, lty = 1)



## Q8
library(ISLR)
fix(Carseats)
attach(Carseats)
train = sample(nrow(Carseats), nrow(Carseats)/2)
x.train = Carseats[train,]
x.test = Carseats[-train,]

?tree
tree.Carseats <- tree(Sales~., Carseats,
                      subset = train)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats, pretty = 0)
pred.carseats <- predict(tree.Carseats, x.test)
mean((Carseats$Sales[-train] - pred.carseats)^2)

#c
?cv.tree
cv.carseats <- cv.tree(tree.Carseats, FUN = prune.tree)
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
?plot
# Best size 10
prune_carseats <- prune.tree(tree.Carseats, best = 10)
par(mfrow = c(1, 1))
plot(prune_carseats)
text(prune_carseats, pretty = 0)
pred.pruned = predict(prune_carseats, x.test)
mean((Carseats$Sales[-train] - pred.pruned)^2)
# Increased

##### BAGGING
?randomForest
bag_carseats <- randomForest(Sales~., data = x.train, mtry = 10, 
                             ntree = 500, importance = T )
bag.pred <- predict(bag_carseats, x.test)
mean((Carseats$Sales[-train] - bag.pred)^2)
importance(bag_carseats)
rf.carseats <- randomForest(Sales~., data = x.train, mtry = 5, 
                            ntree = 500, importance = T)
rf_pred <- predict(rf.carseats, x.test)
mean((Carseats$Sales[-train] - rf_pred)^2)
importance(rf.carseats)




########### Q9
library(ISLR)
fix(OJ)
attach(OJ)
train = sample(nrow(OJ), 800)
x.test = OJ[-train, -1]
?tree
tree.OJ <- tree(Purchase~., data = OJ, subset = train)
summary(tree.OJ)
plot(tree.OJ)
text(tree.OJ)

tree.pred = predict(tree.OJ, x.test, type = "class")
table(tree.pred, OJ$Purchase[-train])
(141+84)/(270)
?cv.tree
cv.OJ = cv.tree(tree.OJ, FUN = prune.tree)
plot(cv.OJ$size, cv.OJ$dev, type = "b")

# optimal size is 5
# Pruning the tree
prune_OJ = prune.tree(tree.OJ, best = 5)
summary(prune_OJ)

pred_tree = predict(prune_OJ, x.test, type = "class")
table(pred_tree, OJ$Purchase[-train])
(131+84)/270
missclass_pred <- sum(pred_tree != OJ$Purchase[-train])
missclass_pred = missclass_pred/length(pred_tree)
missclass_pred_unp <- sum(tree.pred != OJ$Purchase[-train])
missclass_pred_unp = missclass_pred_unp/length(tree.pred)



fix(Hitters)
?na.omit
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)
train <- sample(nrow(Hitters), 200)
train.x = Hitters[train,]
test.x = Hitters[-train,]
library(gbm)
?gbm
?seq
shrink_para <- seq(-10, -0.2, by = 0.1)
shrink_para = 10^shrink_para
mse_train <- rep(0, length(shrink_para))
mse_test <- rep(0, length(shrink_para))


for (i in 1:length(shrink_para)) {
  boost_hitters <- gbm(Salary~., distribution = "gaussian",
                       data = train.x, n.trees = 1000, 
                       shrinkage = shrink_para[i] )
  train_pred <- predict(boost_hitters, train.x, n.trees = 1000)
  test_pred <- predict(boost_hitters, test.x, n.trees = 1000)
  mse_train[i] = mean((train.x$Salary - train_pred)^2)
  mse_test[i] = mean((test.x$Salary - test_pred)^2)
  
}

plot(shrink_para, mse_train, type = "b", col = "blue", 
     xlab = "Shrinkage", ylab = "MSE",main = "MSE against Shrinkage")
plot(shrink_para, mse_test, type = "b", col = "blue", 
     xlab = "Shrinkage", ylab = "MSE",main = "MSE against Shrinkage")


# Q11

library(ISLR)
rm(Caravan)
fix(Caravan)
attach(Caravan)

Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
head(Purchase)


train <- sample(nrow(Caravan), 1000)
train.x <- Caravan[train,]
test.x <- Caravan[-train,]

# b
?gbm
library(gbm)
gbm_caravan <- gbm(Purchase~., data = train.x, distribution = "bernoulli",
                   n.trees = 1000, shrinkage = 0.01 )
summary(gbm_caravan)

# c
caravan_pred <- predict(gbm_caravan, test.x, type = "response",
                        n.trees = 1000)
caravan_prob <- rep("0", nrow(test.x))
caravan_prob[caravan_pred > 0.2] = "1"
table(caravan_prob, test.x$Purchase)
Error_rate <- (144+263)/4822 * 100
33/(144+33)

# Compare results with KNN or Log
# logistic
?glm
log_caravan <- glm(Purchase~., family = binomial, data = train.x)
log_pred <- predict(log_caravan, test.x, type = "response")
log_pred <- ifelse(log_pred > 0.2, 1, 0)
table(log_pred, test.x$Purchase)
50/(50+260) * 100
# Lower than boosting

# KNN
library(class)
?knn
train_knn <- train.x[,-86]
test_knn <- test.x[,-86]
train_lab <- train.x$Purchase
knn_pred = knn(train_knn, test_knn, train_lab, k = 5)
table(knn_pred, test.x$Purchase)

