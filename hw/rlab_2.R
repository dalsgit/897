library (ISLR)
set.seed (3)
train=sample (392, 196)
train
lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train )
attach (Auto)
# MSE for linear
mean((mpg - predict (lm.fit ,Auto))[-train ]^2)

lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
# MSE for quadratic
mean((mpg - predict (lm.fit2 ,Auto))[-train ]^2)

lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
# MSE for cubic
mean((mpg - predict (lm.fit3 ,Auto))[-train ]^2)

#Q2
library (boot)
glm.fit=glm(mpg~poly(horsepower, 6),data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
cv.err$delta

#Q4
set.seed (17)
cv.error.10= rep (0 ,10)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower, i),data=Auto)
  cv.error.10[i]=cv.glm (Auto ,glm.fit ,K=5) $delta [1]
}
cv.error.10

#Q5
set.seed (2)
alpha.fn=function (data ,index){
  X=data$X [index]
  Y=data$Y [index]
  return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}

boot(Portfolio ,alpha.fn, R=1000)

#Q6
boot.fn=function (data ,index )
  coefficients(lm(mpg~horsepower +I( horsepower ^2) ,data=data , subset =index))
set.seed (2)
boot(Auto ,boot.fn ,1000)
summary (lm(mpg~horsepower +I(horsepower ^2) ,data=Auto))$coef
