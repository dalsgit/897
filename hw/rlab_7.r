setwd("C:\\study\\897\\hw")

#install.packages ('pls')
library(ISLR) 
library (pls)

Hitters=na.omit(Hitters)
set.seed (2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE , validation ="CV")
summary (pcr.fit )
validationplot(pcr.fit ,val.type="MSEP")
MSEP(pcr.fit)

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

pcr.fit=pcr(Salary~., data=Hitters ,subset =train ,scale =TRUE , validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")
MSEP(pcr.fit)

pcr.pred=predict (pcr.fit ,x[test ,], ncomp =7)
mean((pcr.pred -y.test)^2)

pcr.pred=predict (pcr.fit ,x[test ,], ncomp =8)
mean((pcr.pred -y.test)^2)

set.seed (1)
pls.fit=plsr(Salary~., data=Hitters ,subset =train ,scale=TRUE ,
               validation ="CV")
summary (pls.fit )
validationplot(pls.fit ,val.type="MSEP")
MSEP(pls.fit)

pls.pred=predict (pls.fit ,x[test ,], ncomp =2)
mean((pls.pred -y.test)^2)

pls.pred=predict (pls.fit ,x[test ,], ncomp =3)
mean((pls.pred -y.test)^2)

pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE ,ncomp =2)
summary (pls.fit )

pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE ,ncomp =3)
summary (pls.fit )
