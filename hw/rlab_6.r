setwd("C:\\study\\897\\hw")

states =row.names(USArrests )
states
names(USArrests )
apply(USArrests , 2, mean)
apply(USArrests , 2, var)
pr.out =prcomp (USArrests , scale =TRUE)
names(pr.out )
pr.out$center
pr.out$scale
pr.out$rotation

pr.out =prcomp (USArrests , scale =FALSE)
pr.out$rotation

pr.out =prcomp (USArrests , scale =TRUE)
pr.out$x
biplot (pr.out , scale =0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

pr.out$sdev
pr.var =pr.out$sdev ^2
sum(pr.var)
sum(apply(scale(USArrests),2,var))

cumsum(pr.var)
summary(pr.out)

library (ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

dim(nci.data)
nci.labs [1:4]
table(nci.labs)
pr.out =prcomp (nci.data , scale=TRUE)

Cols=function (vec ){
  cols=rainbow (length (unique (vec )))
  return (cols[as.numeric (as.factor (vec))])
  }

par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z3")

plot(nci.data[,1:2], col=Cols(nci.labs), pch=19,xlab="X1",ylab="X2")
plot(nci.data[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="X1",ylab="X3")

summary (pr.out)
pr.var =pr.out$sdev ^2
pve=pr.var/sum(pr.var)

plot(pve , xlab="Principal Component", 
     ylab="Proportion of Variance Explained", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained",
     ylim=c(0,1), type='b')
