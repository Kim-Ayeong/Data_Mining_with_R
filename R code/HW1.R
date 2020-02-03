#https://rpubs.com/ahmedtadde/ISLR-chap2-ex9

setwd("C:/workspace/R/데이터마이닝/datasets")

#(a)
Auto = read.table("Auto.data", header=T, na.strings="?") # 첫번째 행을 header로 설정, ?를 결측값으로 인식
Auto = na.omit(Auto)
head(Auto) # 올바르게 읽음
summary(Auto)

#(b)
names(Auto)
attach(Auto)
range(mpg); range(cylinders); range(displacement); range(horsepower)
range(weight); range(acceleration); range(year); range(origin)

#(c)
mean(mpg); sd(mpg)
mean(cylinders); sd(cylinders)
mean(displacement); sd(displacement)
mean(horsepower); sd(horsepower)
mean(weight); sd(weight)
mean(acceleration); sd(acceleration)
mean(year); sd(year)
mean(origin); sd(origin)

#(d)
dim(Auto)
Auto_sub = Auto[-c(10:85), ]
dim(Auto_sub)
detach(Auto)
attach(Auto_sub)
range(mpg); mean(mpg); sd(mpg)
range(cylinders); mean(cylinders); sd(cylinders)
range(displacement); mean(displacement); sd(displacement)
range(horsepower); mean(horsepower); sd(horsepower)
range(weight); mean(weight); sd(weight)
range(acceleration); mean(acceleration); sd(acceleration)
range(year); mean(year); sd(year)
range(origin); mean(origin); sd(origin)

#(e)
detach(Auto_sub)
attach(Auto)
pairs(Auto)
#상관관계가 있어 보이는 변수만 다시 plot
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

#(f)
pairs(Auto)
cor(Auto[, 1:8])
#-1 ~ -0.7 : 강한 음의 상관관계
#-0.7 ~ 0.3 : 뚜렷한 음의 상관관계
#0.3 ~ 0.7 : 뚜렷한 양의 상관관계
#0.7 ~ 1 : 강한 양의 상관관계
cor.test(mpg, cylinders)
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
cor.test(mpg, displacement)
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
cor.test(mpg, horsepower)
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
cor.test(mpg, weight) 
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
cor.test(mpg, acceleration) 
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
cor.test(mpg, year) 
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
cor.test(mpg, origin) 
#p<0.05이므로 귀무가설 기각(=상관관계가 있다.)
