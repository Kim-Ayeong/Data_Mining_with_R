#0910

version #버전 확인

#install.packages("installr") #upgrade
#library(installr)
#install.R()

ls() # 메모리에 있는 변수와 함수 객체 리스트
rm(x,y) # 객체 x,y 삭제
rm(list=ls()) # 객체 전체 삭제
ls()

#Na : not available, 결측값
#NaN : not a number, 0/0의 경우

#Vactor 객체
x <- c(1,3,2,5)
x = c(1,6,2) #변수 재할당
y = c(1,4,3)
x[1] #R만 인덱스 1부터 시작
x[2:3]
length(x)
length(y)
x+y # 길이가 같은 Vector일 경우 원소별로 합할 수 있음
union(x, y) # 합집합 xUy
setdiff(x, y) # 차집합 x-y
intersect(x, y) # 교집합 x∩y

#Matrix 객체
x = matrix(data=c(1,2,3,4), nrow=2, ncol=2) # number of rows=2, number of columns=2 
x
x = matrix(c(1,2,3,4), 2, 2) # R의 도움말 파일과 같은 순서일 경우 인자이름 명시 안해도 됨
matrix(c(1,2,3,4), 2, 2, byrow=TRUE) # byrow=TRUE는 row를 우선으로 채움
sqrt(x) # 각 원소 제곱근
x^2 # 각 원소 제곱
x*x # 각 원소 곱
x%*%x # 행렬의 곱

A = matrix(1:16, 4, 4) # 4x4 matrix 객체 생성
A
A[2,3] # 2행 3열 원소
A[1:3, 2:4] # 1-3행 2-4열 matrix
A[-c(1,3),] # 1행, 3행 제외하고 나머지 matrix
dim(A) # dimensión of matrix A

#정규분포 난수
x = rnorm(50) # 50 random samples from N(0,1^2)
y = x+rnorm(50, mean=50, sd=.1) # 50 random samples from N(50,0.1^2)
cor(x, y) # correlation between x and y

set.seed(3) # 동일한 랜덤 샘플을 얻기위해 seed number 부여
y = rnorm(100) # 100 random samples from N(0,1^2)
mean(y) # 평균
var(y) # 분산
sqrt(var(y)) # 표준편차
sd(y) # 표준편차

#산점도
x = rnorm(1000)
y = rnorm(1000)
plot(x, y) # scatter plot 그림
plot(x, y, xlab="x-axis", ylab="y-axis", main="X vs Y")

getwd()
#pdf("Figure.pdf") # 파일 형태로 저장 
xy = matrix(c(x,y), length(x), 2) # length(x) * 2
head(xy) # 확인
length(xy) # 2000
length(densCols(xy)) # 1000
plot(xy, col=densCols(xy), xlab="x-axis", ylab="y-axis", pch=20, cex=2, main="X vs Y") # density에 따라 색 변경, pch:점 모양, cex:점 크기
dev.off() #plot 화면 지우기

#3차원 그래프
x = y = 1:5 # 5개 원소의 Vector 객체
f = outer(x, y) # x%*%t(y), 외적과 유사
f

contour(x, y, f) # Contour plot(등고선)
image(x, y, f) # Heatmap
persp(x, y, f) # Perspective plot(시각적)

x = y = seq(-pi, pi, length=50) # from, to, length 대신 by도 가능
f = outer(x, y, function(x,y) cos(y)/(1+x^2))
#fun = function(x, y) {cos(y)/(1+x^2)}
#fun(1, 1)
#f = outer(x, y, fun) 와 같은 결과
length(f)
dim(f)
fa = (f-t(f))/2

contour(x, y, f) # Contour plot
image(x, y, f) # Heatmap
persp(x, y, f) # Perspective plot

contour(x, y, fa)
image(x, y, fa)
persp(x, y, fa)

persp(x, y, fa, theta=30, phi=20) # theta:경도, phi:위도
persp(x, y, fa, theta=30, phi=40)
persp(x, y, fa, theta=70, phi=40)

#파일 읽고 쓰기
setwd("C:/workspace/R/데이터마이닝/datasets")
Auto = read.table("Auto.data") # Auto.data 읽기
head(Auto) # 올바르게 읽지 않음
Auto = read.table("Auto.data", header=T, na.strings="?") # 첫번째 행을 header로 설정, ?를 결측값으로 인식
head(Auto) # 올바르게 읽음
summary(Auto)

Auto = read.csv("Auto.csv", header=T, na.strings="?") 
head(Auto)
dim(Auto)

Auto = na.omit(Auto) # NA가 포함된 7개 행을 제거 함
dim(Auto)
write.csv(Auto, "Auto_remove_na.csv", row.names=F) # NA가 제거된 테이블 저장, index 저장 X

sum(is.na(Auto))

#산점도, 박스 플랏
plot(Auto$cylinders, Auto$mpg)
attach(Auto) # Data frame내의 변수들을 사용할 수 있게 함
plot(cylinders, mpg) # 산점도

cylinders = as.factor(cylinders) # 범주형 변수로 변환
head(cylinders)
plot(cylinders, mpg) # 박스도표
plot(cylinders, mpg, col="red", varwidth=T) # widths proportional to the square root of the samples sizes
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)

#히스토그램, 산점도 행렬
hist(mpg) # Histogram
hist(mpg, col=2)
hist(mpg, col=2, breaks=15) 

pairs(Auto) # Scatter plot Matrix
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg) # Scatter plot
identify(horsepower, mpg, name) # 그래프 상의 점에 대한 특정 변수값을 보여줌 오른쪽 클릭으로 종료

summary(cylinders) # 범주형 변수에 대한 요약
summary(mpg) # 연속형 변수에 대한 요약



