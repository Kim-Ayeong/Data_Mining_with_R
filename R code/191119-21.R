# (1) Best Subset Selection	
library(ISLR)

head(Hitters, 2)
dim(Hitters)
summary(Hitters)
fix(Hitters) # 데이터 편집기

sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters) # remove NA
dim(Hitters)
sum(is.na(Hitters)) # 0

# functions for model selection
library(leaps)

regfit.full = regsubsets(Salary~., Hitters) # performs best subset
?regsubsets
summary(regfit.full)
class(regfit.full)
names(regfit.full)

regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19) # nvmax: maximum size of subsets to examine (19-variable model)
reg.summary = summary(regfit.full)
summary(reg.summary)
class(reg.summary)
names(reg.summary)
reg.summary$rsq

# Plot for RSS, Adjusted Rsq, Cp(AIC), BIC
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l") # l:점없이 선으로만 plot

plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max(reg.summary$adjr2) # 가장 큰 값의 index
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type='l')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type='l')
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col="red", cex=2, pch=20) 
# BIC는 변수가 더 많은 모델에 패널티(2에서 ln로 변환)를 부여, AIC보다 더 적은 변수 개수가 선택됨

# Plot for the selected variables for the best model
plot(regfit.full, scale="r2") # 검정:모델에 변수가 선택됐을 때
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full, 6) # 6-variable model has the smallest BIC
?coef.regsubsets # 여기서의 coef를 선택해줌

# (2) Forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="forward") # 추가되는 형태
summary(regfit.fwd) # forward stepwise selection

regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="backward") # 제거되는 형태
summary(regfit.bwd) # backward stepwise selection

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# (3) Model selection using validation set approach
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), replace=TRUE) # random sample with replacement
# train = sort(sample(1:nrow(Hitters), nrow(Hitters)/2)) # 이 방법이 더 편리
test = (!train)
sum(test)/nrow(Hitters)*100 # 49.04%, 보통 50% 확률

regfit.best = regsubsets(Salary~., data=Hitters[train, ], nvmax=19) # 19-variable model
test.mat = model.matrix(Salary~., data=Hitters[test, ]) # X matrix from test data
val.errors = rep(NA, 19)
for(i in 1:19){
  coefi = coef(regfit.best, id=i) # coefficients from i-variable model
  pred = test.mat[, names(coefi)]%*%coefi # Xbeta:행렬곱
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2) # Test-MSE
}
val.errors
which.min(val.errors) # 7

# 선택된 변수로 다시 전체 데이터로 모델 fitting
regfit.best = regsubsets(Salary~., data=Hitters, nvmax=19) # perform on the full data 
coef(regfit.best, id=7)

# (4) Model selection using K-fold CV
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace=TRUE)
table(folds)
cv.errors = matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))
colnames(cv.errors) = 1:19

# predict function for regsubset object
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]]) # extracted the formula used in the call
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  pred = mat[, names(coefi)] %*% coefi
  pred
}

# 위 코드 : regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19)
regfit.full$call[[1]] # 변수의 1번째 인자(속성) 반환
regfit.full$call[[2]]
regfit.full$call[[3]]

for(j in 1:k){
  best.fit = regsubsets(Salary~., data=Hitters[folds!=j, ], nvmax=19) # training data
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds==j, ], id=i) # test data
    cv.errors[j, i] = mean((Hitters$Salary[folds==j]-pred)^2) # test MSE
  }
}
pred
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

reg.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 10)

# (5) Ridge Regression, 대용량 데이터에도 빠르게 돌아가고 결과도 좋아 자주 쓰임
library(glmnet)

x = model.matrix(Salary~., Hitters[, -1])
dim(x)
y = Hitters$Salary
length(y)
grid = 10^seq(10, -2, length=100)

ridge.mod = glmnet(x, y, alpha=0, lambda=grid) # alpha=1 이면 라쏘, 0~1 사이 값이면 앨라스틱넷
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60] # 60번째로 돌려본 결과
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

predict(ridge.mod, s=50, type="coefficients")[1:20, ]
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train, ], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test, ])

mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred = predict(ridge.mod, s=1e10, newx=x[test, ])
mean((ridge.pred-y.test)^2)

ridge.pred = predict(ridge.mod, s=0, newx=x[test, ], exact=T, x=x[train, ], y=y[train])
mean((ridge.pred-y.test)^2)

lm(y ~ x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients", x=x[train, ], y=y[train])[1:20, ]

# select best lambda using k-fold cv
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test, ])
mean((ridge.pred-y.test)^2)

# obtain ridge coefficients with all data
out = glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20, ]

# (6) Lasso
lasso.mod = glmnet(x[train, ], y[train], alpha=1, lambda=grid) 
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
log(bestlam) # plot에 점선으로 표시되어 있음

lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test, ])
mean((lasso.pred-y.test)^2) # Ridge가 더 작음, Lasso는 bias가 조금 더 큼 
out = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef!=0]

# (7) Principal Components Regression
library(pls)

set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit) # MSEP가 가장 낮은 주성분 개수 선택

set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV") # 릿지, 라쏘 모두 scaling 필요 
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)

pcr.pred = predict(pcr.fit, x[test, ], ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit = pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

# (8) Partial Least Squares
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
summary(pls.fit)

pls.pred = predict(pls.fit, x[test, ], ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit = plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)

