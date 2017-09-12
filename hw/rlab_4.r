setwd("C:\\study\\897\\hw")

library (ISLR)
fix(Hitters )
names(Hitters )
sum(is.na(Hitters$Salary))
Hitters =na.omit(Hitters )
dim(Hitters )
#install.packages('leaps')
library (leaps)
regfit.full=regsubsets(Salary~.,Hitters ,nvmax =19, method = 'exhaustive')
reg.summary =summary (regfit.full)
reg.summary$rsq[9]
reg.summary$adjr2[9]
reg.summary$cp[9]
reg.summary$bic[9]
names(reg.summary )

regfit.fwd=regsubsets (Salary~.,data=Hitters ,nvmax =19,
                        method ="forward")
summary (regfit.fwd )

regfit.bwd=regsubsets (Salary~.,data=Hitters ,nvmax =19,
                        method ="backward")
summary (regfit.bwd )

?regsubsets
regfit.seqrep=regsubsets (Salary~.,data=Hitters ,nvmax =19,
                       method ="seqrep")
summary (regfit.seqrep )

set.seed (1)
train=sample (c(TRUE ,FALSE), nrow(Hitters ),rep=TRUE)
test =(! train )
regfit.best=regsubsets (Salary~.,data=Hitters [train ,],
                         nvmax =19)
test.mat=model.matrix (Salary~.,data=Hitters [test ,])

val.errors =rep(NA ,19)
for(i in 1:19){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat [,names(coefi)]%*% coefi
  val.errors [i]= mean(( Hitters$Salary[test]-pred)^2)
}

val.errors
which.min (val.errors )
coef(regfit.best ,10)

predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix (form ,newdata )
  coefi =coef(object ,id=id)
  xvars =names (coefi )
  mat[,xvars ]%*% coefi
}

val.errors_2 =rep(NA ,19)
for(i in 1:19){
  pred=predict(regfit.best,Hitters[test,],i)
  val.errors_2 [i]= mean(( Hitters$Salary[test]-pred)^2)
}
val.errors_2 - val.errors

k=5
set.seed (1)
folds=sample (1:k,nrow(Hitters ),replace =TRUE)
cv.errors =matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))

for(j in 1:k){
  best.fit =regsubsets (Salary~.,data=Hitters [folds !=j,], nvmax =19)
  for(i in 1:19) {
    pred=predict (best.fit ,Hitters [folds ==j,], id=i)
    cv.errors [j,i]=mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
}

mean.cv.errors =apply(cv.errors ,2, mean)
mean.cv.errors
min(mean.cv.errors)
par(mfrow =c(1,1))
plot(mean.cv.errors ,type='b')
reg.best=regsubsets (Salary~.,data=Hitters , nvmax =19)
coef(reg.best ,10)
