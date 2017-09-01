library(stats)
library(boot)
library (ISLR)

head(Auto)
dim(Auto)
train = sample(392, 196)

train.error=rep (0,9)
test.error=rep (0,9)

for (i in 1:9){
  lm.fit = lm(mpg ~ poly(displacement + horsepower + weight + acceleration, i), data=Auto, subset=train)
  train.error[i] = mean((mpg - predict (lm.fit, Auto))[train ]^2)
  test.error[i] = mean((mpg - predict (lm.fit, Auto))[-train ]^2)
}
warnings()
train.error
test.error
plot(x = 1:9, y = train.error, type='b',xlab = "Flexibility - Polynomial Degree", ylab = "Error", 
      main = "Training error keeps going down", col="grey")
lines(x = 1:9, y = test.error, type='b', col="red")
