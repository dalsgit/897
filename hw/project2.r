setwd("C:\\study\\897\\hw")
#setwd("C:\\study\\psu\\git\\897\\hw") 

suppressWarnings(library(MASS))
library(class)
library(glmnet)
library(leaps)
library(caret)
library(ggplot2)
library(tidyverse)
library(corrplot)

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

df = read.csv("proj2_amesHousing.txt", sep = "\t")

# drop the rows that are outliers as explained in the data description
plot(df$SalePrice, df$Gr.Liv.Area)
df <- df[Gr.Liv.Area<=4000,]
plot(df$SalePrice, df$Gr.Liv.Area)
dim(df)

#remove columns we dont need for the model
df = df[ , -which(names(df) %in% c("Order","PID"))]
dim(df)

#for easy looking, let's plot them separately in factor and numeric data set
#numeric data set
num = sapply(df, is.numeric)
numdat= df [, num]
numdat%>%bar_missing()

#factor data set
fac= sapply(df, is.factor)
facdat= df [, fac]
facdat%>%bar_missing()


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

#"GarageYrBlt". It is logical that the garages were built same time with the houses. How GarageYrBlt and YearBuilt of the houses looks like
length(which(df$Garage.Yr.Blt == df$Year.Built)) / dim(df)[1]
idx <- which(is.na(df$Garage.Yr.Blt))
df[idx, "Garage.Yr.Blt"] <- df[idx, "Year.Built"]

#MasVnrType (Masonry veneer type) & MasVnrArea (Masonry veneer area in square feet) are related to each other
table(Mas.Vnr.Type)
df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")]
count(df[(is.na(Mas.Vnr.Type)) | (is.na(Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")])
df$Mas.Vnr.Type[Mas.Vnr.Type==''] = "None"
df$Mas.Vnr.Area[is.na(df$Mas.Vnr.Area)] = 0
df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")]

# square footage - basement and living areas
count(df[Total.Bsmt.SF!=BsmtFin.SF.1+BsmtFin.SF.2+Bsmt.Unf.SF,c("Bsmt.Unf.SF","BsmtFin.SF.1","BsmtFin.SF.2","Total.Bsmt.SF")])
df[Gr.Liv.Area!=X1st.Flr.SF+X2nd.Flr.SF+Low.Qual.Fin.SF,c("X1st.Flr.SF","X2nd.Flr.SF","Low.Qual.Fin.SF","Gr.Liv.Area")]

# Merge the square footage (basement, 1st and 2nd floors) and remove columns
df$tot.sqft <- df$Total.Bsmt.SF + df$Gr.Liv.Area
df = df[ , -which(names(df) %in% c("Bsmt.Unf.SF","Gr.Liv.Area"))]

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
corrplot.mixed(corr.matrix)

#near-zero-variance
nzv.data = nearZeroVar(df, saveMetrics = TRUE)
drop.cols = rownames(nzv.data)[nzv.data$nzv == TRUE]
df = df[,!names(df) %in% drop.cols]
dim(df)

## Neighborhood
table(df$Neighborhood)
# GrnHill and Landmrk neighborhoods have very less representation. removing these rows
# df <-  filter(Neighborhood != "GrnHill" & Neighborhood != "Landmrk")

n=nrow(df)
set.seed(7736)
test = sample(n, round(n/4)) ## train indices are the rest
train = setdiff(1:n, test)

df.train = df[ train,]
df.test = df[test,]

# Let's start with parameter selection for the Boston data set. 
# We will use forward selection, lasso and ridge here:

x = model.matrix(SalePrice ~ . - 1, data = df)
y = df$SalePrice

p = ncol(df) - 1

train.mat <- model.matrix(SalePrice~ ., data = df.train)
test.mat <- model.matrix(SalePrice~ ., data = df.test)

# Forward Selection | BIC
regfit.fwd=regsubsets (SalePrice~.,data=df.train, nvmax=p, method='forward')
reg.summary = summary (regfit.fwd)
reg.summary
plot(reg.summary$bic, xlab ="Number of Variables",ylab="BIC", type = 'l', main = 'Forward Step - Performance Measure')
which.min (reg.summary$bic )
points (which.min (reg.summary$bic ), reg.summary$bic[which.min (reg.summary$bic )], col ="red",cex =2, pch =20)

# Best model
regfit.best=regsubsets (SalePrice~.,data=df.train, nvmax=p, really.big = T)
test.mat=model.matrix (SalePrice~.,data=df.test)
test.val.errors =rep(NA ,p)
for(i in 1:17){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat [,names(coefi)] %*% coefi
  test.val.errors [i]= mean(( df.test$SalePrice-pred)^2)
}
plot(test.val.errors ,type='b', xlab='# of parameters', ylab='Test MSE')

#LASSO
grid =10^ seq (10,-2, length =100)
cv.lasso = cv.glmnet(x, y, type.measure = "mse", nfolds=10)
plot(cv.lasso)
bestlam.lasso=cv.lasso$lambda.min #find the best tuning parameter

fit.lasso <- glmnet(train.mat, df.train$SalePrice, alpha = 1, lambda = grid, thresh = 1e-12)
pred.lasso=predict (fit.lasso, s=bestlam.lasso, newx=test.mat)
mean(( pred.lasso - df.test$SalePrice)^2)

final.lasso=glmnet(x,y,alpha=1) #fit on the entire data set to extract coef
lasso.coef=predict(final.lasso,type="coefficients",s=bestlam.lasso)[1:14,]
lasso.coef
length(lasso.coef[lasso.coef !=0])
lasso.coef[lasso.coef!=0] #contains 11 variables in our model

#Ridge regression
cv.ridge = cv.glmnet(x, y, alpha=0, type.measure = "mse", nfolds=length(y),grouped=FALSE)
plot(cv.ridge)
bestlam.ridge=cv.ridge$lambda.min #find the best tuning parameter

fit.ridge =glmnet(train.mat, train$crim, alpha = 0, lambda = grid, thresh = 1e-12)
pred.ridge = predict (fit.ridge, s=bestlam.ridge, newx=test.mat)
mean(( pred.ridge - test$crim)^2)

final.ridge=glmnet(x,y,alpha=0) #fit on the full data
ridge.coef=predict(final.ridge,type="coefficients",s=bestlam.ridge)[1:14,]
ridge.coef
ridge.coef[ridge.coef!=0] #contains all variables in our model

### Start modeling with the selected parameters ###

# First add the classification column
sp_modified <- rep(0, length(SalePrice))
sp_modified[which(SalePrice >= 200000)] <- 1

df <- data.frame(df, sp_modified)
dim(df)


