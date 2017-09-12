setwd("C:\\study\\897\\hw")

library(MASS)
library(stats)
library(boot)
library (ISLR)

Advertising = read.csv("Advertising.csv")
lm.fit =lm(Sales~TV, data = Advertising)
confint (lm.fit, level = 0.95)

attach(Boston)
lm.fit =lm(medv~lstat)
confint (lm.fit, level = 0.95)
predict (lm.fit ,data.frame(lstat=c(20) ), interval ="prediction")
names(Boston)

lm.fit1=lm(medv~.-age ,data=Boston )
summary(lm.fit1)
r=rstudent(lm.fit1)
write.csv(r, file = "temp.csv")

h=hatvalues(lm.fit1)
write.csv(h, file = "temp.csv")

c=cooks.distance(lm.fit1)
write.csv(c, file = "temp.csv")

lm.model1=lm(medv~lstat *age ,data=Boston )
lm.model2=lm(medv~lstat *black ,data=Boston )
summary(lm.model1)
summary(lm.model2)

summary (lm(medv~log(rm),data=Boston ))

lm.fit =lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
summary (lm.fit)

lm.fit =lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats, contrasts=list(ShelveLoc=contr.treatment(c("Bad", "Good", "Medium"), base=3)))
summary (lm.fit)


library(mlbench)
library(ISLR)
data("BostonHousing")
attach(BostonHousing)

lm.fit = lm(medv~., data=BostonHousing)
summary(lm.fit)

pairs(BostonHousing)
nums <- sapply(BostonHousing, is.numeric)
cor(BostonHousing[,nums])
#crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat   

lm.fit1 = lm(log(medv)~crim+chas+nox+rm+dis+rad+tax+ptratio+b+sqrt(lstat), data=BostonHousing)
summary(lm.fit1)
plot(lm.fit1, which = 1)

lm.fit2 = lm(medv~.^2, data=BostonHousing)
summary(lm.fit2)

names(BostonHousing)
