############### TREESS #######################
#### Library tree is used
install.packages("tree")
library(tree)
library(ISLR)
fix(Carseats)
attach(Carseats)

?ifelse
high <- ifelse(Sales > 8, "Yes", "No")
?data.frame
Carseats <- data.frame(Carseats, high)
?tree
str(Carseats)
tree.carseats <- tree(high~.-Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

# Test error
set.seed(2)
train = sample(nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
high.test <- high[-train]
tree.carseats = tree(high~. -Sales, Carseats, subset = train)
?predict
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, high.test)
(104+50)/200 * 100

?cv.tree
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats$size
cv.carseats$dev
cv.carseats$k
# tree with 21 terminal nodes results in lowest CV error rate, with 74 CV errors
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
?plot
plot(cv.carseats$k, cv.carseats$dev, type = "b")
?prune.misclass
# Pruning the tree
prune.carseats <- prune.misclass(tree.carseats, best = 21)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
?text
prune.carseats
# effectiveness of pruned tree on Test data
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, high.test)
(104+53)/(200) * 100


#### Fitting Regression Tree to BostonData set
library(MASS)
set.seed(1)
fix(Boston)
attach(Boston)
train = sample(nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)
plot(tree.boston)
par(mfrow=c(1,1))
text(tree.boston, pretty = 0)
cv.boston = cv.tree(tree.boston)
names(cv.boston)
cv.boston$dev
plot(cv.boston$size, cv.boston$dev, type = 'b')
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
yhat <- predict(tree.boston, Boston[-train,])
boston.test=Boston$medv[-train]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)


##### Bagging and Random Forest
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv~., Boston, subset = train, mtry = 13, 
                          important = TRUE)

bag.boston
yhat.bag = predict(bag.boston, Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)


# Change the number of trees drawn by RF
bag.boston = randomForest(medv~., Boston, subset = train, mtry = 13, 
                          important = TRUE, ntree = 25)

bag.boston
yhat.bag = predict(bag.boston, Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)


# Default RF
set.seed(1)
rf.boston = randomForest(medv~., Boston, subset = train, mtry = 6, 
                         importance = TRUE)
yhat.rf = predict(rf.boston, Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)




############ BOOSTING
install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston = gbm(medv~., Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
par(mfrow = c(1,2))
plot(boost.boston, i="rm")
?plot
plot(boost.boston, i="lstat")
yhat.boost = predict(boost.boston, Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)


# Modify Alpha i.e. penalty
boost.boston = gbm(medv~., Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.2,
                   verbose = F)
yhat.boost = predict(boost.boston, Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
