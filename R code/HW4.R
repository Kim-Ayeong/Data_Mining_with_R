#8장 10번 

library(ISLR)

head(Hitters) #Salary를 예측하는 것이 목표
dim(Hitters)
summary(Hitters) #결측값이 Salary에만 있음

# (a)
sum(is.na(Hitters))
Hitters = na.omit(Hitters)
sum(is.na(Hitters))

summary(Hitters$Salary)
Hitters$Salary = log(Hitters$Salary)
summary(Hitters$Salary)

# (b)
train = Hitters[1:200, ]
test = Hitters[201:dim(Hitters)[1], ]
dim(train); dim(test)

# (c)
library(gbm)

set.seed(20)
shr_lambda = seq(0, 0.5, 0.01)
MSE_train = rep(0, length(shr_lambda))

for (i in 1:length(shr_lambda)) {
  boost_hitters = gbm(Salary~., data=train, distribution="gaussian", 
                      n.trees=1000, shrinkage=shr_lambda[i], verbose=F)
  yhat_train = predict(boost_hitters, train, n.trees=1000)
  MSE_train[i] = mean((yhat_train - train$Salary)^2)
}
plot(shr_lambda, MSE_train, type="b", xlab="Shrinkage values", ylab="Training MSE")

# (d)
set.seed(20)
shr_lambda = seq(0, 0.5, 0.005)
MSE_test = rep(0, length(shr_lambda))

for (i in 1:length(shr_lambda)) {
  boost_hitters = gbm(Salary~., data=train, distribution="gaussian", 
                      n.trees=1000, shrinkage=shr_lambda[i], verbose=F)
  yhat_test = predict(boost_hitters, test, n.trees=1000)
  MSE_test[i] = mean((yhat_test - test$Salary)^2)
}
plot(shr_lambda, MSE_test, type="b", xlab="Shrinkage values", ylab="Testing MSE")

# (e)
min(MSE_test)

# 3장
linear_fit = lm(Salary~., train)
linear_pred = predict(linear_fit, test)
MSE1 = mean((test$Salary-linear_pred)^2); MSE1

# 6장
x_train = model.matrix(Salary~., train)[, -1]
y_train = train$Salary
x_test = model.matrix(Salary~., test)[, -1]
y_test = test$Salary

library(glmnet)

set.seed(20)
cv_out = cv.glmnet(x_train, y_train, alpha=0)
bestlam = cv_out$lambda.min
ridge_fit = glmnet(x_train, y_train, alpha=0, lambda=bestlam)
ridge_pred = predict(ridge_fit, s=bestlam, newx=x_test)
MSE2 = mean((y_test-ridge_pred)^2); MSE2

cv_out = cv.glmnet(x_train, y_train, alpha=1)
bestlam = cv_out$lambda.min
lasso_fit = glmnet(x_train, y_train, alpha=1, lambda=bestlam)
lasso_pred = predict(lasso_fit, s=bestlam, newx=x_test)
MSE3 = mean((y_test-lasso_pred)^2); MSE3

# (f)
summary(boost_hitters)

# (g)
library(randomForest)

set.seed(20)
bag_hitters = randomForest(Salary~., data=train, mtry=19, ntree=500)
yhat_bag = predict(bag_hitters, test)
mean((yhat_bag-test$Salary)^2)
