install.packages("rmarkdown")
install.packages("ISLR")
library(ISLR)
x=c(1,2,3,3)
summary(x)

set.seed(3) 
y=rnorm(100)
summary(y)

?plot

x3=matrix(1:12,3)
x3
x3[,2]

fix(Auto)
attach (Auto)
cylinders =as.factor (cylinders )
plot(cylinders , mpg)

#install.packages("doBy")
library(doBy)
summaryBy(mpg ~ cylinders, data = Auto, 
          FUN = list(mean, max, min, median, sd))

hist(mpg)
hist(mpg ,col =2)
hist(mpg ,col =2, breaks =15)

pairs(??? mpg + displacement + horsepower + weight +
         acceleration , Auto)
cor(Auto[sapply(Auto, is.numeric)])

plot(horsepower ,mpg)
identify (horsepower ,mpg ,name)
