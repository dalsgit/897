library(lars)
data("diabetes")
attach(diabetes)
data.all = data.frame(cbind(diabetes$x, y = diabetes$y))
str(data.all)
summary(data.all)
anyNA(data.all)

###FORMATTING FOR MODEL BUILDING###
n <- dim(data.all)[1] 
seed = 38723#1306 38723
set.seed(seed) 
test <- sample(n, round(n/4), replace = FALSE)
sum(test)
data.train <- data.all[-test,]
data.test <- data.all[test,]
x <- model.matrix(y~ ., data = data.all)[,-1]

x.train <- x[-test,]
x.test <- x[test,]
y <- data.all$y
y.train <- y[-test]
y.test <- y[test]
n.train <- dim(data.train)[1] 
n.test <- dim(data.test)[1]

###LEAST SQUARES LINEAR REGRESSION###
lm.full = lm(y~. , data = data.train)
round(coef(summary(lm.full)),2)
pred = predict(lm.full, data.test)
cat(paste0("The test MSE for the full linear regression model is: ",round(mean((y.test - pred)^2),2)))
plot(lm.full$fitted.values,resid(lm.full), xlab="Fitted Values", ylab="Residuals", main="Residual Plot", pch=21, col="blue")
abline(h=0.0, lwd=2, lty=2)

###BEST SUBSETS USING BIC###
library(leaps)
set.seed(seed) 
regfit.full = regsubsets(y~. , data = data.train , nvmax = 10)
reg.summary = summary(regfit.full)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
x = which.min(reg.summary$bic)
y = reg.summary$bic[x]
points(x,y, col="red", cex=1, pch=19)
cat(names(coef(regfit.full,x)))
round(coef(summary(lm(y~sex+bmi+map+hdl+ltg, data = data.train))),2)
coefi  = coef(regfit.full,id=x)
pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1])
cat(paste0("The test MSE for the best subsets model is: ",round(mean((y.test - pred)^2),2)))

###BEST SUBSETS USING 10-FOLD CROSS VALIDATION###
predict.regsubsets = function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

k=10
set.seed(seed)
folds = sample(1:k, nrow(data.train),replace = TRUE)
cv.errors = matrix(NA,k,10, dimnames = list(NULL, paste(1:10)))

for (j in 1:k){
  best.fit = regsubsets(y~. ,data=data.train[folds!=j,],nvmax = 10)
  for (i in 1:10){
    pred = predict.regsubsets(best.fit, data.train[folds==j,], id=i)
    cv.errors[j,i] = mean((data.train$y[folds==j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
plot(mean.cv.errors, type="b", main = "Mean CV errors",xlab = "Number of Predictors",
     ylab="Mean CV Errors")


y = min(mean.cv.errors)
x = which.min(mean.cv.errors)

points(x,y, col="red", cex=1, pch=19)

regfit.cv = regsubsets(y~. , data = data.train , nvmax = 10)
coefi  = coef(regfit.cv,id=5)
pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1])
coefi
cat(paste0("The test MSE for the best subsets model using CV is: ",round(mean((y.test - pred)^2),2)))


###RIDGE REGRESSION USING CROSS VALIDATION###
library(glmnet)
grid = 10^seq(10,-2,length=100)
set.seed(seed)
cv.out = cv.glmnet(x.train, y.train, alpha = 0)
largelam = cv.out$lambda.1se
ridge.mod = glmnet(x.train, y.train, alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s=largelam, newx=x.test)
cat(paste0("The test MSE for the ridge regression model using CV is: ",round(mean((y.test - ridge.pred)^2),2)))
coef = glmnet(x.train, y.train, alpha = 0, lambda = largelam, thresh = 1e-12)$beta
matrix(coef, dimnames = list(row.names(coef), c("Coefficient")))

###LASSO MODEL USING CROSS VALIDATION###
set.seed(seed)
cv.out = cv.glmnet(x.train, y.train, alpha = 1)
largelam = cv.out$lambda.1se
lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = largelam)
lasso.pred = predict(lasso.mod, s=largelam, newx = x.test)
cat(paste0("The test MSE for the lasso model using CV is: ",round(mean((y.test - lasso.pred)^2),2)))

coef= glmnet(x.train, y.train, alpha = 1, lambda = largelam)$beta
matrix(coef, dimnames = list(row.names(coef), c("Coefficient")))
