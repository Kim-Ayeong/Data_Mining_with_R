# (1) Smarket 데이터
#install.packages("ISLR")
library(ISLR)
library(help="ISLR") # ISLR 패키지 정보
dim(Smarket) # S&P Stock Market Data
head(Smarket) # Y = Direction, 주가 등락
cor(Smarket[, -9])

attach(Smarket)
plot(Volume, col="blue", pch=16, main="Plot for Volumn in Smarket Data")

# (2) Fit Logistic Regression
colnames(Smarket)
#fit1 = lm(Today~Lag1+Lag2, data=Smarket); coef(fit1)
#fit2 = glm(Today~Lag1+Lag2, data=Smarket); coef(fit2) 
# 결과가 같음, 로지스틱이 제대로 적용되지 않음, family=binomial, link="logit 지정
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef # 같은 결과

# (3) Get predicted probability
glm.probs = predict(glm.fit, type="response") # p=p(Y=1|X), 확률값
# type = c("link", "response", "terms")
# type = "link": values of f(y) based on like function
# type = "response": values of P(Y=1|X)
# type = "terms": a matrix given fit of each observation
head(glm.probs)
glm.probs1 = predict(glm.fit, type="link") # link=log(p/(1-p/))
head(glm.probs1)
log(head(glm.probs)/(1-head(glm.probs))) # link 함수

contrasts(Direction) # factor(범주형, 질적변수) 보기
#contrasts(as.factor(1:4))
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
#glm.pred = rep("Up", 1250); glm.pred[glm.probs <= 0.5] = "Down" 도 가능
table(glm.pred)
table(Direction)
table(glm.pred, Direction) #yhat, y 순서
prop.true = (507+145)/1250
prop.true # 52.16% 적중도 
# 임의 추측보다 조금 좋은 정도
# training error rate = 100-52.16 =47.84%
# how to compute test error rate? 

t = table(glm.pred, Direction)
(t[1,1] + t[2,2])/sum(t) # TP+TN/Total = accuracy

# (4) Fit logistic regression using training data 
summary(Smarket)
train = (Year < 2005)
table(train)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005) # n_test=252
Direction.2005 = Direction[!train]
length(Direction.2005)
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train) # train은 Boolean 타입
glm.probs = predict(glm.fit, newdata=Smarket.2005, type="response") # newdata를 test데이터로 설정

glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
prop.true = mean(glm.pred == Direction.2005) # True(TP+TN)/Total = accuracy
prop.true
prop.false = mean(glm.pred != Direction.2005)
prop.false # test error rate = 51.98% (=(97+34)/252), 임의 추측보다 더 안좋음

# (5) Fit logistic regression using training data with only Lag1 and Lag2
glm.fit = glm(Direction ~ Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type="response")
glm.pred = rep("Down",252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005) 
prop.true = mean(glm.pred == Direction.2005)
prop.true # 단순한 모델을 넣으니 결과가 약간 나아져 56% 올바르게 예측함

# (6) Fit LDA using training data
library(MASS)
lda.fit = lda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
lda.fit #P(Y=1)=phi1, P(Y=0)=phi0 : prior
# linear discriminants: f=-0.64*Lag1-0.51*Lag2
# 491/(491+507) : 첫번째 prior
#507/(491+507) : 두번째 prior

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
# class: LDA’s predictions on the movement of the market
# posterior: a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class
# x: the linear discriminants

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
# LDA는 56%를 올바르게 예측 Logsitic 회귀와 비슷

# (7) Fit QDA using training data
library(MASS)
qda.fit = qda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) # QDA는 거의 60% 예측

# (8) Fit KNN using training data
library(class)
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

set.seed(1) # set seed number 
knn.pred = knn(train.X, test.X, train.Direction, k=1)
# train.X: training 데이터의 설명 변수
# test.X: test 데이터의 설명 변수
# train.Direction: training 데이터의 반응 변수
# k: KNN 방법의 K값
table(knn.pred, Direction.2005)
prop.true = (83+43)/252
prop.true # 50% 예측률, 결과 안좋음

# (9) Fit KNN with K=3
knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
prop.true = mean(knn.pred == Direction.2005)
prop.true # 예측률이 53%로 조금 나아짐

# (10) Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase) # 반응 변수가 Purchase, 6% 정도만 보험 구매
# Caravan Insurance data consists of 86 variables, containing sociodemographic data (variables 1-43) and product ownership (variables 44-86)

# (11) Fit KNN using Caravan Data
standardized.X = scale(Caravan[, -86]) # 스케일 문제를 해결하기 위해 standardization
test = 1:1000 # 처음 1000개 test 데이터
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1) # set seed number
knn.pred = knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.Y)
mean(test.Y != knn.pred) # 오차율 11.8% 임 
mean(test.Y != "No") # 모두 No라고 예측해도 오차율 6%
9/(68+9) # 보험 구입을 예측한 77명중 9명 즉 11.6%가 실제로 구매 임의 추측 (6%) 보다 2배 높음
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
5/26 # K=3으로 바꾸면 성공률이 19.2%로 증가
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
4/15 # K=5로 바꾸면 성공률이 26.7%로 증가

# (12) Fit Logistic using Caravan Data
glm.fit = glm(Purchase ~ ., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test, ], type="response")
glm.pred = rep("No",1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)
# cut-off for the classifier 를 0.5 설정하면 7명만 보험 구입을 예측한다 (too conservative!!)

glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)
11/(22+11) # cut-off for the classifier 를 0.25 설정하면 33명을 보험 구입자로 예측하고 그중 11명이 실제 보험 구입을 함 (33%)
