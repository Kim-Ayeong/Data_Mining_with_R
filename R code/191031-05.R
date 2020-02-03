# (1) Classification Trees using training data
#install.packages("tree") # install the tree package
library(tree)
library(ISLR)

attach(Carseats)
str(Carseats)
High = ifelse(Sales<=8, "No", "Yes") # Sales (quantitative) -> High (qualitative)
Carseats = data.frame(Carseats, High) 
tree.carseats = tree(High ~ .-Sales, Carseats) # fit classification tree
summary(tree.carseats)

plot(tree.carseats) #27개 terminal node는 너무 많음
text(tree.carseats, pretty=0) 
# pretty=0: include the category names for any qualitative predictors 

tree.carseats

# (2) Classification Trees using test error rate
set.seed(3)
train = sort(sample(1:nrow(Carseats), 200))
test = setdiff(1:400,train) #400개 중 train 200개 제외
Carseats.test = Carseats[test, ] # test data set
High.test = High[test] # response variable for test data
tree.carseats = tree(High ~ .-Sales, Carseats, subset=train) # fit tree with only training data
tree.pred = predict(tree.carseats, Carseats.test, type="class") # type=“class”: predicted class
table(tree.pred, High.test)
(88+48)/200 # correctly predicted 
# large tree까지 만드는 과정

# (3) CV+pruning for classification tree
set.seed(6)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
# cv.tree: cv to determine the optimal level of tree complexity
# FUN=prune.misclass: classification error rate to guide the CV and pruning process
names(cv.carseats)
cv.carseats # size:terminal node 개수, dev:CV error rate, k:alpha(커지면 terminal node가 작아짐)

par(mfrow=c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type="b") # CV error rate
plot(cv.carseats$k, cv.carseats$dev, type="b") # cost-complexity parameter

prune.carseats = prune.misclass(tree.carseats, best=12) # pruning for tree with 12 terminal nodes
plot(prune.carseats)
text(prune.carseats, pretty=0, cex=0.8)

tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(87+54)/200 # correctly predicted 

rm(list=ls())
dev.off()
par(mfrow=c(1, 1))

# (4) Regression Trees using training data
library(MASS)
set.seed(1)
head(Boston)
train = sort(sample(1:nrow(Boston), nrow(Boston)/2)) # index for training data
test = setdiff(1:nrow(Boston), train) # index for test data
tree.boston = tree(medv ~ ., Boston, subset=train) # fit tree using training data
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0, cex=0.8)

# (5) CV+pruning for Regression tree
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
prune.boston = prune.tree(tree.boston, best=7)

plot(prune.boston)
text(prune.boston, pretty=0, cex=0.8) #마지막 값은 해당되는 y값을 평균낸 값

yhat = predict(tree.boston, newdata=Boston[test, ])
boston.test = Boston[test, "medv"] # y
plot(yhat, boston.test)
abline(0, 1)

mean((yhat-boston.test)^2) # test MSE

# (6) Bagging 
#install.packages("randomForest") # install package randomForest
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~ ., data=Boston, subset=train, mtry=13, importance=T)
# mtry=13: all 13 predictors should be considered for each split of the tree (i.e. bagging)
# importance=TRUE: Should importance of predictors be assessed?
bag.boston

yhat.bag = predict(bag.boston, newdata=Boston[test, ])
plot(yhat.bag, boston.test)
abline(0, 1)

mean((yhat.bag-boston.test)^2) # smaller than regression tree

bag.boston = randomForest(medv ~ ., data=Boston, subset=train, mtry=6, ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[test, ])
mean((yhat.bag-boston.test)^2) 

# (7) Random Forests
set.seed(1)
rf.boston = randomForest(medv ~ ., data=Boston, subset=train, mtry=13, importance=TRUE)
# mtry=6: 6 predictors should be considered for each split of the tree (i.e. random forest)
yhat.rf = predict(rf.boston, newdata=Boston[-train, ])
mean((yhat.rf-boston.test)^2) # smallest

importance(rf.boston)
varImpPlot(rf.boston)

# (8) Boosting
#install.packages("gbm") # install package gbm
library(gbm)
set.seed(1)
boost.boston = gbm(medv ~ ., data=Boston[train, ], distribution="gaussian", n.trees=5000, interaction.depth=4) 
# regression tree, 5000 trees, interaction.depth=4 limits the depth of each tree
summary(boost.boston) # rm and lstat are the most important

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata=Boston[test, ], n.trees=5000)
mean((yhat.boost-boston.test)^2) # best

boost.boston = gbm(medv ~ ., data=Boston[train, ], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F) 
# the shrinkage parameter λ=0.2
yhat.boost = predict(boost.boston, newdata=Boston[test, ], n.trees=5000)
mean((yhat.boost-boston.test)^2)
