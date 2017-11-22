library (ISLR)

set.seed (2)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4

km.out =kmeans (x,2, nstart =20)
km.out$cluster

plot(x, col =(km.out$cluster +1) , main="K-Means Clustering
Results with K=2", xlab ="", ylab="", pch =20, cex =2)

km.out =kmeans (x,3, nstart =20)
km.out$cluster

plot(x, col =(km.out$cluster +1) , main="K-Means Clustering
     Results with K=2", xlab ="", ylab="", pch =20, cex =2)

set.seed (4)
km.out =kmeans (x,3, nstart =20)
km.out

plot(x, col =(km.out$cluster +1) , main="K-Means Clustering
Results with K=3", xlab ="", ylab="", pch =20, cex =2)

set.seed (3)
km.out =kmeans (x,3, nstart =1)
km.out$tot.withinss
km.out =kmeans (x,3, nstart =20)
km.out$tot.withinss

km.out =kmeans (x,3, nstart =50)
km.out$tot.withinss

# Hierarchical clustering
hc.complete =hclust (dist(x), method ="complete")
hc.average =hclust (dist(x), method ="average")
hc.single =hclust (dist(x), method ="single")

par(mfrow =c(1,3))
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="",
       cex =.9)
plot(hc.average , main =" Average Linkage ", xlab="", sub ="",
       cex =.9)
plot(hc.single , main=" Single Linkage ", xlab="", sub ="",
       cex =.9)

cutree (hc.complete , 2)
cutree (hc.average , 2)
cutree (hc.single , 2)

plot(x, col=(cutree(hc.complete, 2)+1), main="2-cluster Hierarchical Results for Complete Linkage", xlab="", ylab="", pch=20, cex=2)
plot(x, col=(cutree(hc.average, 2)+1), main="2-cluster Hierarchical Results for Average Linkage", xlab="", ylab="", pch=20, cex=2)
plot(x, col=(cutree(hc.single, 2)+1), main="2-cluster Hierarchical Results for Single Linkage", xlab="", ylab="", pch=20, cex=2)

plot(x, col=(cutree(hc.single, 4)+1), main="4-cluster Hierarchical Results for Single Linkage", xlab="", ylab="", pch=20, cex=2)


nci.labs=NCI60$labs
nci.data=NCI60$data

nci.labs [1:4]
table(nci.labs)
sd.data=scale(nci.data)

par(mfrow =c(1,3))
data.dist=dist(sd.data)
plot(hclust (data.dist), labels =nci.labs , main=" Complete
       Linkage ", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="average"), labels =nci.labs ,
       main=" Average Linkage ", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="single"), labels =nci.labs ,
       main=" Single Linkage ", xlab="", sub ="", ylab ="")

hc.out =hclust (dist(sd.data))
hc.clusters =cutree (hc.out ,4)
table(hc.clusters ,nci.labs)

par(mfrow =c(1,1))
plot(hc.out , labels =nci.labs)
abline (h=139, col =" red ")
hc.out

set.seed (2)
km.out =kmeans (sd.data , 4, nstart =20)
km.clusters =km.out$cluster
table(km.clusters ,hc.clusters )

table(km.clusters,nci.labs)

