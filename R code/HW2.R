#https://rpubs.com/ahmedtadde/ISLR-chap3-ex9

setwd("C:/workspace/R/데이터마이닝/datasets")

#library("ISLR")
Auto = read.table("Auto.data", header=T, na.strings="?") # 첫번째 행을 header로 설정, ?를 결측값으로 인식
Auto = na.omit(Auto)
attach(Auto)

#(a)
pairs(Auto)

#(b)
cor(Auto[, names(Auto)!="name"])

#(c)
model = lm(mpg ~. -name, data = Auto)
summary(model)

#(d)
par(mfrow = c(2,2))
plot(model)

#(e)
model = lm(mpg ~.-name + cylinders:displacement, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + cylinders:horsepower, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + cylinders:weight, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + cylinders:acceleration, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + cylinders:origin, data=Auto); summary(model) #유의X

model = lm(mpg ~.-name + displacement:horsepower, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + displacement:weight, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + displacement:acceleration, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + displacement:origin, data=Auto); summary(model) #유의O

model = lm(mpg ~.-name + horsepower:weight, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + horsepower:acceleration, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + horsepower:origin, data=Auto); summary(model) #유의O

model = lm(mpg ~.-name + weight:acceleration, data=Auto); summary(model) #유의O
model = lm(mpg ~.-name + weight:origin, data=Auto); summary(model) #유의O

model = lm(mpg ~.-name + acceleration:origin, data=Auto); summary(model) #유의O

#-----

model = lm(mpg ~.-name + displacement:weight, data = Auto)
summary(model)

model = lm(mpg ~.-name + cylinders:displacement + displacement:weight + horsepower:acceleration, data=Auto)
summary(model)

model = lm(mpg ~.-name + cylinders:displacement + displacement:weight + horsepower:acceleration + year:origin, data=Auto)
summary(model)

model = lm(mpg ~.-name - cylinders - acceleration + displacement:weight + horsepower:acceleration + weight:acceleration + year:origin, data=Auto)
summary(model)

#(f)
model = lm(mpg ~. -name + I(cylinders^2) + I(displacement^2) + sqrt(horsepower) + log(weight), data=Auto)
summary(model)
