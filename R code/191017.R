# (1) Validation Set Approach
library(ISLR) # load library
attach(Auto)
head(Auto)
dim(Auto)

set.seed(1) 
train = sort(sample(1:392, 196)) # size=196, 반반 분할
train
test = setdiff(1:392, train) #나머지 값
test

# 1st order
lm.fit = lm(mpg ~ horsepower, data=Auto, subset=train) # fit linear model using only training data set
summary(lm.fit)

y.test = Auto$mpg[test] #test:index값
x.test = Auto[test, -1]
mse.test = function(y, yhat) {
  mean((y - yhat)^2)
}
yhat = predict(lm.fit, x.test)
mse.test(y.test, yhat) # compute test MSE
# mean((mpg - predict(lm.fit, Auto))[-train]^2) 도 가능

# 2nd order
poly(horsepower, 2, raw=T) # default=False
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train) # quadratic
summary(lm.fit2)

yhat = predict(lm.fit2, x.test)
mse.test(y.test, yhat) # compute test MSE

# 3rd order
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train) # cubic
summary(lm.fit3) # 3차가 유의하지 않음

yhat = predict(lm.fit3, x.test)
mse.test(y.test, yhat) # compute test MSE
# Model that predicts mpg using a quadratic function of horsepower performs better than a model that involves only a linear function of horsepower
# Little evidence in favor of a model that uses a cubic function of horsepower

# (2) LOOCV(Leave-One-Out Cross Validation)
lm.fit = lm(mpg ~ horsepower, data=Auto)
coef(lm.fit)
glm.fit = glm(mpg ~ horsepower, data=Auto)
coef(glm.fit) #  # lm과 glm 결과가 같음, family="gaussian"

library(boot) # CV for GLM
cv.err = cv.glm(Auto, glm.fit) #자동으로 LOOCV 수행
#cv.glm(data, glmfit, cost, K) : cost=cost function for CV(default cost=MSE), K=number of groups into which the data should be split(default K=N)
cv.err$delta # 첫번째 인자가 MSE, 하나씩 뺀 test를 n번 반복하므로 전체 n으로 MSE가 계산됨
cv.error = rep(0, 5)

# 1ts~5th order
for (i in 1:5) {
  print(i)
  glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(cv.error, type='o')
# sharp drop in the estimated test MSE between the linear and quadratic fits, but no clear improvement from using higher-order polynomials

# (3) k-fold CV
set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10) {
  print(i)
  glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
plot(cv.error.10, type='o')
# little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply using a quadratic fit 

# (4) Boostrap
library(ISLR)
head(Portfolio) # simple simulated data
dim(Portfolio)

# 통계량 함수
alpha.fn = function(data, index) { 
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y))) #return 생략 가능
}
alpha.fn(Portfolio, 1:100) # alpha 추정치

set.seed(2)
sort(sample(1:100, 100, replace=T))
alpha.fn(Portfolio, sample(1:100, 100, replace=T)) # random sampling with replacement

boot(Portfolio, alpha.fn, R=1000)
# boot function produces R = 1000 bootstrap estimates for α
# α_hat=0.5758321(평균값), se(α _hat)=0.093(표준편차)

# (5) Accuracy of Linear Regression Model
boot.fn = function(data, index) {
  res = coef(lm(mpg ~ horsepower, data=data, subset=index))
  res
}
boot.fn(Auto, 1:392) # Auto data의 전체 1:392행을 lm 함수로 fitting해서 intercept와 slope를 얻음
set.seed(1) # 반복할 때마다 다른 값 출력
boot.fn(Auto, sample(392, 392, replace=T))
boot.fn(Auto, sample(392, 392, replace=T)) #돌릴 때마다 값이 다름

boot(Auto, boot.fn, 1000) # boostrap
summary(lm(mpg ~ horsepower, data=Auto))$coef # linear model
# the estimates from lm are somewhat different from the estimates obtained using the bootstrap
# Does this indicate a problem with the bootstrap? NO!(붓스트랩의 표준편차가 조금 더 높지만 문제가 있지는 않음)
# (why? Because sigma^2 estimates are not accurate due to non-linearity)

boot.fn = function(data, index) {
  coef(lm(mpg ~ poly(horsepower, 2), data=data, subset=index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ poly(horsepower, 2), data=Auto))$coef # 결과가 같음
