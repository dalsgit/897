#install.packages('tree')
library (tree)
library (ISLR)
data("Carseats")
attach (Carseats )
High=ifelse (Sales <=8," No"," Yes ")
Carseats =data.frame(Carseats ,High)
tree.carseats =tree(High~.-Sales ,Carseats )
summary (tree.carseats )

plot(tree.carseats )
text(tree.carseats ,pretty =0)
tree.carseats 
set.seed (2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train ]
tree.carseats =tree(High~.-Sales ,Carseats ,subset =train )
tree.pred=predict (tree.carseats ,Carseats.test ,type ="class")
table(tree.pred ,High.test)

set.seed (3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )

cv.carseats
par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
prune.carseats =prune.misclass (tree.carseats ,best =9)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
(94+60) /200
prune.carseats =prune.misclass (tree.carseats ,best =15)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
(86+62) /200

prune.carseats =prune.misclass (tree.carseats ,best =13)
tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
(91+63) /200

library (MASS)
set.seed (1)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
tree.boston =tree(medv~.,Boston ,subset =train)
summary (tree.boston )
plot(tree.boston )
text(tree.boston ,pretty =0)

cv.boston =cv.tree(tree.boston )
plot(cv.boston$size ,cv.boston$dev ,type='b')

prune.boston =prune.tree(tree.boston ,best =5)
plot(prune.boston )
text(prune.boston ,pretty =0)

yhat=predict (tree.boston ,newdata =Boston [-train ,])
boston.test=Boston [-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

# Bagging, Boosting, Random Forests
library (randomForest)
set.seed (1)
bag.boston =randomForest(medv~.,data=Boston ,subset=train,mtry=13, importance =TRUE)
bag.boston
yhat.bag = predict (bag.boston ,newdata =Boston [-train ,])
plot(yhat.bag , boston.test)
abline (0,1)
mean(( yhat.bag -boston.test)^2)
bag.boston =randomForest(medv~.,data=Boston ,subset =train ,
                             mtry=13, ntree =25)
yhat.bag = predict (bag.boston ,newdata =Boston [-train ,])
mean(( yhat.bag -boston.test)^2)
set.seed (1)
rf.boston =randomForest(medv~.,data=Boston ,subset =train ,
                          mtry=6, importance =TRUE)
yhat.rf = predict (rf.boston ,newdata =Boston [-train ,])
mean(( yhat.rf -boston.test)^2)

importance (rf.boston )
varImpPlot (rf.boston )

set.seed(1)
rf.boston =randomForest(medv~.,data=Boston ,subset =train ,importance =TRUE)
yhat.rf = predict (rf.boston ,newdata =Boston [-train ,])
mean(( yhat.rf -boston.test)^2)

importance (rf.boston )
varImpPlot (rf.boston )

# Boosting
# install.packages('gbm')
library (gbm)
set.seed (1)
boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4)
summary (boost.boston )
par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                        n.trees =5000)
mean(( yhat.boost -boston.test)^2)
boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,
                    verbose =F)
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                      n.trees =5000)
mean(( yhat.boost -boston.test)^2)

plot(boost.boston ,i="dis")
set.seed(1)
boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                    "gaussian",
                  n.trees=5000, interaction.depth=4, shrinkage=0.01,
                  verbose =F)
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                    n.trees =5000)
mean(( yhat.boost -boston.test)^2)

set.seed(1)
boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                    "gaussian",
                  n.trees=5000, interaction.depth=3, shrinkage=0.01,
                  verbose =F)
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                    n.trees =5000)
mean(( yhat.boost -boston.test)^2)


q11data=readRDS("q11data.Rda")
q11data$Y <- as.factor(q11data$Y)
str(q11data)

tree.q11 =tree(Y~., q11data)
summary (tree.q11 )

plot(tree.q11 )
text(tree.q11 ,pretty =0)

df_test<-data.frame(0, 14, 14, 0.1, 0.1, 0.1)
names(df_test)<-c("Y", "X1", "X2", "X3", "X4", "X5")

predict (tree.q11, df_test,type ="class")
