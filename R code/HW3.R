# 6장 9번

library(ISLR)
head(College) #Apps 예측하는 것이 목표
dim(College)
sum(is.na(College))

# (a)
set.seed(9)
train_i = sort(sample(1:nrow(College), 400))
test_i = (-train)
train = College[train_i, ]
test = College[test_i, ]
dim(train); dim(test)

# (b)
set.seed(9)
linear_fit = lm(Apps ~ ., train)
summary(linear_fit)
linear_pred = predict(linear_fit, test)
MSE = mean((test$Apps-linear_pred)^2); MSE

# (c)
library(glmnet)
x_train = model.matrix(Apps ~ ., train)[, -1]
y_train = train$Apps
x_test = model.matrix(Apps ~ ., test)[, -1]
y_test = test$Apps

set.seed(9)
cv_out = cv.glmnet(x_train, y_train, alpha=0)
plot(cv_out)
bestlam = cv_out$lambda.min; bestlam

ridge_fit = glmnet(x_train, y_train, alpha=0, lambda=bestlam)
ridge_fit$beta
ridge_pred = predict(ridge_fit, s=bestlam, newx=x_test)
MSE2 = mean((y_test-ridge_pred)^2); MSE2

# (d)
set.seed(9)
cv_out = cv.glmnet(x_train, y_train, alpha=1)
plot(cv_out)

bestlam = cv_out$lambda.min; bestlam

lasso_fit = glmnet(x_train, y_train, alpha=1, lambda=bestlam)
lasso_fit$beta
lasso_pred = predict(lasso_fit, s=bestlam, newx=x_test)
MSE3 = mean((y_test-lasso_pred)^2); MSE3

lasso_coef = predict(lasso_fit, type="coefficients", s=bestlam)[1:length(lasso_fit$beta),]
lasso_coef[lasso_coef!=0]

# (e)
library(pls)

set.seed(9)
pcr_fit = pcr(Apps ~ ., data=College, subset=train_i, scale=TRUE, validation="CV") # 릿지, 라쏘 모두 scaling 필요 
validationplot(pcr_fit, val.type="MSEP") # MSEP가 가장 낮은 주성분 개수 선택
summary(pcr_fit)

pcr_pred = predict(pcr_fit, x_test, ncomp=10)
MSE4 = mean((y_test-pcr_pred)^2); MSE4

# (f)
set.seed(9)
pls_fit = plsr(Apps ~ ., data=College, subset=train_i, scale=TRUE, validation="CV")
validationplot(pls_fit, val.type="MSEP")
summary(pls_fit)

pls_pred = predict(pls_fit, x_test, ncomp=10)
MSE5 = mean((y_test-pls_pred)^2); MSE5

# (g)
result = data.frame(model=c('Linear','Ridge', 'Lasso', 'PCR', "PLS"), 
                    MSE=c(MSE, MSE2, MSE3, MSE4, MSE5)); result
result[result$MSE == min(result$MSE), ]

