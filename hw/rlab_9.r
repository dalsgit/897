setwd("C:\\study\\897\\hw")

library (ISLR)
names(Smarket )
attach (Smarket )
plot(Volume )

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
               data=Smarket ,family =binomial )
summary (glm.fits)

train =(Year <2005)
Smarket.2005= Smarket [! train ,]
dim(Smarket.2005)
Direction.2005= Direction [! train]

glm.fits=glm(Direction~Lag1+Lag2 ,data=Smarket ,family =binomial ,subset =train)
summary (glm.fits)
glm.probs =predict (glm.fits,Smarket.2005 , type="response")
glm.pred=rep ("Down " ,252)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred ,Direction.2005)
contrasts(Direction.2005)

library (MASS)
lda.fit=lda(Direction~Lag1+Lag2 ,data=Smarket ,subset =train)
lda.fit
plot(lda.fit)
lda.pred=predict (lda.fit , Smarket.2005)
names(lda.pred)

lda.class =lda.pred$class
table(lda.class ,Direction.2005)
table(Direction.2005, lda.class)

min(lda.pred$posterior [,1])

qda.fit=qda(Direction~Lag1+Lag2 ,data=Smarket ,subset =train)
qda.fit
qda.class =predict (qda.fit ,Smarket.2005) $class
table(Direction.2005, qda.class)


library (class)
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind (Lag1 ,Lag2)[!train ,]
train.Direction =Direction [train]

set.seed (1)
knn.pred=knn (train.X,test.X,train.Direction ,k=1)
table(knn.pred ,Direction.2005)
set.seed(1)
knn.pred=knn (train.X,test.X,train.Direction ,k=3)
table(knn.pred ,Direction.2005)

dim(Caravan )
attach (Caravan )
summary (Purchase )
standardized.X=scale(Caravan [,-86])
test =1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed (1)
knn.pred=knn (train.X,test.X,train.Y,k=1)
mean(test.Y!= knn.pred)
mean(test.Y!="No")
table(knn.pred ,test.Y)
knn.pred=knn (train.X,test.X,train.Y,k=3)
table(knn.pred ,test.Y)
knn.pred=knn (train.X,test.X,train.Y,k=5)
table(knn.pred ,test.Y)

glm.fits=glm(Purchase~.,data=Caravan ,family =binomial ,subset =-test)
glm.probs =predict ( glm.fits, Caravan [test,], type ="response")
glm.pred=rep ("No" ,1000)
glm.pred[glm.probs >.5]="Yes"
table(glm.pred ,test.Y)

glm.pred=rep ("No" ,1000)
glm.pred[glm.probs >.25]=" Yes"
table(glm.pred ,test.Y)

lda.fit=lda(Purchase~.,data=Caravan,subset=-test)
lda.probs=predict(lda.fit, Caravan[test,])$posterior[,2]
lda.pred=rep("No",1000)
lda.pred[lda.probs>.25]="Yes"
table(lda.pred,test.Y)
13/(46+13)
11/(11+48)
