setwd("C:\\study\\897\\hw")

#install.packages('glmnet')
library(glmnet)
library (ISLR)

data(Hitters )
names(Hitters )
sum(is.na(Hitters$Salary))
Hitters =na.omit(Hitters )
sum(is.na(Hitters$Salary))

x=model.matrix (Salary~.,Hitters )[,-1]
y=Hitters$Salary

set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]
grid =10^ seq (10,-2, length =100)

ridge.mod =glmnet(x[train ,],y[train],alpha =0, lambda =grid, thresh=1e-12)
ridge.pred=predict (ridge.mod, s=4, newx=x[test ,])
mean(( ridge.pred -y.test)^2)

ridge.pred=predict (ridge.mod, s=50, newx=x[test ,])
mean(( ridge.pred -y.test)^2)

ridge.mod =glmnet (x,y,alpha =0, lambda =grid)
plot(ridge.mod, xvar="lambda", label=T)

set.seed (1)
ridge.mod =glmnet(x[train ,],y[train],alpha =0, lambda =grid, thresh=1e-12)
cv.out =cv.glmnet (x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean(( ridge.pred -y.test)^2)

oneSElam = cv.out$lambda.1se
oneSElam
ridge.pred=predict (ridge.mod ,s=oneSElam ,newx=x[test ,])
mean(( ridge.pred -y.test)^2)

lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)

oneSElam = cv.out$lambda.1se
oneSElam
lasso.pred=predict (lasso.mod ,s=oneSElam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)
lasso.coef[lasso.coef !=0]

out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef=predict (out ,type ="coefficients",s=oneSElam )[1:20 ,]
lasso.coef
lasso.coef[lasso.coef !=0]
