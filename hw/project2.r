#setwd("C:\\study\\897\\hw")
setwd("C:\\study\\psu\\git\\897\\hw") 

suppressWarnings(library(MASS))
library(class)
library(glmnet)
library(leaps)
library(caret)
library(ggplot2)
library(plyr)
library(tidyselect)

#+++++++++ FUNCTIONS
naisnone= c("Pool.QC", "Misc.Feature", "Alley", "Bsmt.Qual", "Bsmt.Cond", 
            "Bsmt.Exposure", "BsmtFin.Type.1", "BsmtFin.Type.2", "Fireplace.Qu", 
            "Garage.Type", "Garage.Finish", "Garage.Qual", "Garage.Cond", "Fence")

none= function(data, var){
  levels(data[, var]) <- c(levels(data[, var]), "none")
  data[, var][is.na(data[, var])] <- "none"
  return(data[, var])
}

bar_missing = function(x){
  library(dplyr)
  library(reshape2)
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2)) +
    geom_bar(aes(y=(..count..),fill=value),alpha=0.7,color="black")+scale_fill_manual(values=c("gold","red3"),name = "",
                                                                                      labels = c("Available","Missing"))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Observations")+coord_flip()
}

#-------------------

df = read.csv("proj2_amesHousing.txt", sep = "\t", header = TRUE)
str(df)
dim(df)

#remove columns we dont need for the model
df = df[ , -which(names(df) %in% c("Order","PID"))]
dim(df)

#for easy looking, let's plot them separately in factor and numeric data set
#numeric data set
num = sapply(df, is.numeric)
numdat= df [, num]
bar_missing(numdat)

#factor data set
fac= sapply(df, is.factor)
facdat= df [, fac]
bar_missing(facdat)

# drop the rows that are outliers as explained in the data description
plot(df$SalePrice, df$Gr.Liv.Area)
df <- df[df$Gr.Liv.Area<=4000,]
plot(df$SalePrice, df$Gr.Liv.Area)
dim(df)

# first check and clean the data
# Are there any missing values in the data
any(is.na(df))
# How many are there
sum(is.na(df))
# return index of columns that have missing values 
na.cols = which(colSums(is.na(df)) > 0)
# Break down missing values by variable
sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)

#Based on data discription, some of Na's value just mean "house doesn't have it " (not really missing value, just wrong label).So, I will do label those variables in right their categories.
for (i in 1:length(naisnone)){
  df[, naisnone[i]]<- none(df, naisnone[i]) 
}
sum(is.na(df))

# Zoning
table(df$MS.Zoning)
df$MS.Zoning<-as.character(df$MS.Zoning)
# shorten to A
index <- which(df$MS.Zoning == "A (agr)")
df[index, 'MS.Zoning'] <- "A"
# shorten to C
index <- which(df$MS.Zoning == "C (all)")
df[index, 'MS.Zoning'] <- "C"
# Shorten to I
index <- which(df$MS.Zoning == "I (all)")
df[index, "MS.Zoning"] <- "I"
df$MS.Zoning<-factor(df$MS.Zoning)
# results
table(df$MS.Zoning) 

# Lot area
# scatter plot vs sale price
ggplot(df, aes(x = df$Lot.Area, y = df$SalePrice)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") 
# log lot area
ggplot(df, aes(x = log(df$Lot.Area), y = df$SalePrice)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") 
# add variable
df$ln.Lot.Area <- log(df$Lot.Area)
# Delete the columns Lot.Area - keep the transformed column
df = df[ , -which(names(df) %in% c("Lot.Area"))]

# MS.SubClass
# We dropped the MS.SubClass variable as it is a combination of bldg.type, house.style, and year.built.
df = df[ , -which(names(df) %in% c("MS.SubClass"))]
dim(df)

# Bath rooms - merge into single
df$Total.Bath = df$Bsmt.Full.Bath + df$Full.Bath + (.5 * df$Bsmt.Half.Bath) + (.5 * df$Half.Bath)
summary(df$Total.Bath)
df = df[ , -which(names(df) %in% c("Bsmt.Full.Bath", "Full.Bath", "Bsmt.Half.Bath", "Half.Bath"))]
dim(df)

#"GarageYrBlt". 
length(which(df$Garage.Yr.Blt == df$Year.Built)) / dim(df)[1]
table(df$Garage.Type)
df[df$Garage.Type == "none" && df$Garage.Yr.Blt < df$Year.Built, c("Garage.Yr.Blt", "Year.Built")]
# where Garage.Yr.Blt=none and Garage type is none, set the year to 0 representing no garage
idx <- which((is.na(df$Garage.Yr.Blt) & df$Garage.Type=="none"))
df[idx, "Garage.Yr.Blt"] <- 0

# do we have condition where garage is before the house - change garage date = year built 
df[(df$Garage.Yr.Blt < df$Year.Built && df$Garage.Type != "none"), c("Garage.Yr.Blt", "Year.Built")]


#MasVnrType (Masonry veneer type) & MasVnrArea (Masonry veneer area in square feet) are related to each other
table(df$Mas.Vnr.Type)
df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")]
count(df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")])
df$Mas.Vnr.Type<-as.character(df$Mas.Vnr.Type)
df$Mas.Vnr.Type[df$Mas.Vnr.Type==''] = "none"
df$Mas.Vnr.Area[is.na(df$Mas.Vnr.Area)] = 0
df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")]
df$Mas.Vnr.Type<-factor(df$Mas.Vnr.Type)

# square footage - basement and living areas
count(df[df$Total.Bsmt.SF!=df$BsmtFin.SF.1+df$BsmtFin.SF.2+df$Bsmt.Unf.SF,c("Bsmt.Unf.SF","BsmtFin.SF.1","BsmtFin.SF.2","Total.Bsmt.SF")])
df[df$Gr.Liv.Area!=df$X1st.Flr.SF+df$X2nd.Flr.SF+df$Low.Qual.Fin.SF,c("X1st.Flr.SF","X2nd.Flr.SF","Low.Qual.Fin.SF","Gr.Liv.Area")]

# Merge the square footage (basement, 1st and 2nd floors) and remove columns
df$tot.sqft <- df$Total.Bsmt.SF + df$Gr.Liv.Area
df = df[ , -which(names(df) %in% c("Bsmt.Unf.SF","Gr.Liv.Area"))]
# "BsmtFin.SF.2","BsmtFin.SF.1", "X1st.Flr.SF", "X2nd.Flr.SF","Low.Qual.Fin.SF"

# return index of columns that have missing values 
na.cols = which(colSums(is.na(df)) > 0)
# Break down missing values by variable
sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)

# Delete the columns LotFrontage - many missing values
df = df[ , -which(names(df) %in% c("Lot.Frontage"))]

sum(is.na(df))
na.rows = which(rowSums(is.na(df)) > 0)
df <- df[-c(na.rows),]
dim(df)

# At this point we dont have any missing values in the data frame
num = sapply(df, is.numeric)
numdat= df [, num]
corr.matrix = cor(numdat)

#near-zero-variance
#nzv.data = nearZeroVar(df, saveMetrics = TRUE)
#drop.cols = rownames(nzv.data)[nzv.data$nzv == TRUE]
#df = df[,!names(df) %in% drop.cols]
#dim(df)

## Neighborhood
table(df$Neighborhood)
# GrnHill and Landmrk neighborhoods have very less representation. removing these rows
df <- df[!(df$Neighborhood == "GrnHill" | df$Neighborhood == "Landmrk"),]
table(df$Neighborhood)

## Utilities
table(df$Utilities)
# Virtually no variance in the column, drop the column
df = df[ , -which(names(df) %in% c("Utilities"))]

dim(df)


## Parameter selection

# First add the classification column
sp_modified <- rep(0, length(df$SalePrice))
sp_modified[which(df$SalePrice >= 200000)] <- 1
df = df[ , -which(names(df) %in% c("SalePrice"))]

table(sp_modified)

df <- data.frame(df, sp_modified)
dim(df)

n=nrow(df)
set.seed(7736)
test = sample(n, round(n/4)) ## train indices are the rest
train = setdiff(1:n, test)

df.train = df[ train,]
df.test = df[test,]

# Let's start with parameter selection for the Boston data set. 
# We will use forward selection, lasso and ridge here:

x = model.matrix(sp_modified ~ ., data = df)
p = ncol(df) - 1

train.mat <- model.matrix(sp_modified~ ., data = df.train)
test.mat <- model.matrix(sp_modified~ ., data = df.test)

#which(sapply(df.test, function(x) (is.character(x) | is.factor(x)) & length(unique(x))<2))

# Forward Selection | BIC
regfit.fwd=regsubsets (sp_modified~.,data=df.train, nvmax=p, method='forward')
reg.summary = summary (regfit.fwd)
plot(reg.summary$bic, xlab ="Number of Variables",ylab="BIC", type = 'l', main = 'Forward Step - Performance Measure')
which.min (reg.summary$bic )
points (which.min (reg.summary$bic ), reg.summary$bic[which.min (reg.summary$bic )], col ="red",cex =2, pch =20)

coefi=coef(regfit.fwd ,id=which.min (reg.summary$bic ))
pred=test.mat [,names(coefi)] %*% coefi
mean(( df.test$SalePrice-pred)^2)
names(coefi)

plot(reg.summary$cp, xlab ="Number of Variables",ylab="cp", type = 'l', main = 'Forward Step - Performance Measure')
which.min (reg.summary$cp )
points (which.min (reg.summary$cp ), reg.summary$cp[which.min (reg.summary$cp )], col ="red",cex =2, pch =20)

coefi=coef(regfit.fwd ,id=which.min (reg.summary$cp ))
pred=test.mat [,names(coefi)] %*% coefi
mean(( df.test$SalePrice-pred)^2)


regfit.bwd=regsubsets (SalePrice~.,data=df.train, nvmax=p, method='backward')
reg.summary =  summary(regfit.bwd)
plot(reg.summary$bic, xlab ="Number of Variables",ylab="BIC", type = 'l', main = 'Backward Step - Performance Measure')
which.min (reg.summary$bic )
points (which.min (reg.summary$bic ), reg.summary$bic[which.min (reg.summary$bic )], col ="red",cex =2, pch =20)

plot(reg.summary$cp, xlab ="Number of Variables",ylab="cp", type = 'l', main = 'Backward Step - Performance Measure')
which.min (reg.summary$cp )
points (which.min (reg.summary$cp), reg.summary$cp[which.min (reg.summary$cp)], col ="red",cex =2, pch =20)


#LASSO
grid =10^ seq (10,-2, length =100)
cv.lasso = cv.glmnet(x, y=as.factor(df$sp_modified), family="binomial",type.measure = "mse", nfolds=10)
plot(cv.lasso)
bestlam.lasso=cv.lasso$lambda.min #find the best tuning parameter

fit.lasso <- glmnet(train.mat, y=as.factor(df.train$sp_modified), alpha=1, family="binomial", lambda = grid, thresh = 1e-12)
plot(fit.lasso, xvar="lambda")
#fit.lasso <- glmnet(train.mat, df.train$SalePrice, alpha = 1, lambda = grid, thresh = 1e-12)
probs=predict (fit.lasso, s=bestlam.lasso, newx=test.mat)
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
conf_matrix = table(pred.glm, df.test$sp_modified)
conf_matrix
cm=confusionMatrix(data = pred.glm, reference = df.test$sp_modified)
cm$byClass

lasso.coef=predict(fit.lasso,type="coefficients",s=bestlam.lasso)

final.lasso=glmnet(x,y=as.factor(df$sp_modified),alpha=1,family = "binomial") #fit on the entire data set to extract coef
lasso.coef=predict(final.lasso,type="coefficients",s=bestlam.lasso)
lasso.coef=lasso.coef[1:length(lasso.coef),]
length(lasso.coef[lasso.coef !=0])
names(lasso.coef[lasso.coef!=0])


#Ridge regression
cv.ridge = cv.glmnet(x, y=as.factor(df$sp_modified), alpha=0, type.measure="mse", family="binomial", nfolds=10,grouped=FALSE)
plot(cv.ridge)
bestlam.ridge=cv.ridge$lambda.min #find the best tuning parameter

fit.ridge =glmnet(train.mat, y=as.factor(df.train$sp_modified), family="binomial", alpha=0, lambda=grid, thresh=1e-12)
probs = predict (fit.ridge, s=bestlam.ridge, newx=test.mat)
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
conf_matrix = table(pred.glm, df.test$sp_modified)
conf_matrix
cm=confusionMatrix(data = pred.glm, reference = df.test$sp_modified)
cm$byClass

final.ridge=glmnet(x,y,alpha=0) #fit on the full data
ridge.coef=predict(final.ridge,type="coefficients",s=bestlam.ridge)[1:14,]
ridge.coef
ridge.coef[ridge.coef!=0] #contains all variables in our model

# KNN with CV
# all variables left after cleaning
pred.knn <- knn(train.mat, test.mat, df.train$sp_modified, k = 1)
table(pred.knn, df.test$sp_modified)
mean(pred.knn == df.test$sp_modified)
cm=confusionMatrix(data = pred.knn, reference = df.test$sp_modified)
cm$byClass

#Overall.Qual+Exterior.1st+Foundation+Total.Bath+Overall.Cond+Exterior.2nd+Bsmt.Qual+Kitchen.Qual+tot.sqft+Neighborhood+Condition.1+Year.Remod.Add+Bsmt.Exposure+Functional+Open.Porch.SF+Lot.Shape+Condition.2+Exter.Qual+BsmtFin.Type.1+Fireplaces+Garage.Area+ln.Lot.Area
train.mat <- model.matrix(sp_modified~ Overall.Qual+Exterior.1st+Foundation+Total.Bath+Overall.Cond+Exterior.2nd+Bsmt.Qual+Kitchen.Qual+tot.sqft+Neighborhood+Condition.1+Year.Remod.Add+Bsmt.Exposure+Functional+Open.Porch.SF+Lot.Shape+Condition.2+Exter.Qual+BsmtFin.Type.1+Fireplaces+Garage.Area+ln.Lot.Area, data = df.train)
test.mat <- model.matrix(sp_modified~ Overall.Qual+Exterior.1st+Foundation+Total.Bath+Overall.Cond+Exterior.2nd+Bsmt.Qual+Kitchen.Qual+tot.sqft+Neighborhood+Condition.1+Year.Remod.Add+Bsmt.Exposure+Functional+Open.Porch.SF+Lot.Shape+Condition.2+Exter.Qual+BsmtFin.Type.1+Fireplaces+Garage.Area+ln.Lot.Area, data = df.test)

pred.knn <- knn(train.mat, test.mat, df.train$sp_modified, k = 1)
table(pred.knn, df.test$sp_modified)
mean(pred.knn == df.test$sp_modified)
cm=confusionMatrix(data = pred.knn, reference = df.test$sp_modified)
cm$byClass
