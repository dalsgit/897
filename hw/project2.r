#setwd("C:\\study\\897\\hw")
setwd("C:\\study\\psu\\git\\897\\hw") 

suppressWarnings(library(MASS))
library(class)
library(glmnet)
library(leaps)
library(caret)
library(ggplot2)
library(plyr)
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
dim(df)

# drop the rows that are outliers as explained in the data description
plot(df$SalePrice, df$Gr.Liv.Area)
df <- df[df$Gr.Liv.Area<=4000,]
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
table(df$Mas.Vnr.Type)
df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")]
count(df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")])
df$Mas.Vnr.Type[df$Mas.Vnr.Type==''] = "None"
df$Mas.Vnr.Area[is.na(df$Mas.Vnr.Area)] = 0
df[(is.na(df$Mas.Vnr.Type)) | (is.na(df$Mas.Vnr.Area)), c("Mas.Vnr.Type", "Mas.Vnr.Area")]

# square footage - basement and living areas
count(df[df$Total.Bsmt.SF!=df$BsmtFin.SF.1+df$BsmtFin.SF.2+df$Bsmt.Unf.SF,c("Bsmt.Unf.SF","BsmtFin.SF.1","BsmtFin.SF.2","Total.Bsmt.SF")])
df[df$Gr.Liv.Area!=df$X1st.Flr.SF+df$X2nd.Flr.SF+df$Low.Qual.Fin.SF,c("X1st.Flr.SF","X2nd.Flr.SF","Low.Qual.Fin.SF","Gr.Liv.Area")]

# Merge the square footage (basement, 1st and 2nd floors) and remove columns
df$tot.sqft <- df$Total.Bsmt.SF + df$Gr.Liv.Area
df = df[ , -which(names(df) %in% c("Bsmt.Unf.SF","Gr.Liv.Area"))]


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
plot(reg.summary$bic, xlab ="Number of Variables",ylab="BIC", type = 'l', main = 'Forward Step - Performance Measure')
which.min (reg.summary$bic )
points (which.min (reg.summary$bic ), reg.summary$bic[which.min (reg.summary$bic )], col ="red",cex =2, pch =20)

coefi=coef(regfit.fwd ,id=which.min (reg.summary$bic ))
pred=test.mat [,names(coefi)] %*% coefi
mean(( df.test$SalePrice-pred)^2)

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
cv.lasso = cv.glmnet(x, y, type.measure = "mse", nfolds=10)
plot(cv.lasso)
bestlam.lasso=cv.lasso$lambda.min #find the best tuning parameter

fit.lasso <- glmnet(train.mat, df.train$SalePrice, alpha = 1, lambda = grid, thresh = 1e-12)
pred.lasso=predict (fit.lasso, s=bestlam.lasso, newx=test.mat)
mean(( pred.lasso - df.test$SalePrice)^2)

final.lasso=glmnet(x,y,alpha=1) #fit on the entire data set to extract coef
lasso.coef=predict(final.lasso,type="coefficients",s=bestlam.lasso)[1:285,]
length(lasso.coef[lasso.coef !=0])
lasso.coef[lasso.coef!=0]

# Non zero coeff are (74): MS.SubClass + MS.Zoning + Lot.Area + StreetPave + Lot.Shape + Land.Contour + Land.Contour + Utilities + UtilitiesNoSewr + Lot.ConfigCulDSac + Lot.Config + Land.Slope + Neighborhood + Condition.1 + Condition.2 + Bldg.Type + House.Style + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + Roof.StyleHip + Roof.StyleMansard + Roof.Matl + Exterior.1st + Exterior.2nd + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual + Exter.Cond + FoundationCBlock + FoundationPConc + FoundationStone + FoundationWood + Bsmt.QualEx + Bsmt.Qual + Bsmt.Exposure + BsmtFin.Type.1 + BsmtFin.SF.1 + BsmtFin.Type.2 + BsmtFin.SF.2 + Heating + X1st.Flr.SF + X2nd.Flr.SF + Bsmt.Full.Bath + Bsmt.Half.Bath + Full.Bath + Half.Bath + Bedroom.AbvGr + Kitchen.AbvGr + Kitchen.Qual + TotRms.AbvGrd + Functional + Fireplaces + Fireplace.Qu + Garage.Type + Garage.Yr.Blt + Garage.Finish + Garage.Cars + Garage.Area + Garage.Qual + Garage.Cond + Paved.DriveP + Wood.Deck.SF + Open.Porch.SF + Enclosed.Porch + Screen.Porch + Pool.QC + FenceGdWo + Misc.Feature + Yr.Sold + Sale.Type + Sale.Condition + tot.sqft 
#lasso.drops = names(lasso.coef[lasso.coef==0])
#df1 <- df1[ , !(names(df1) %in% lasso.drops)]

#Ridge regression
cv.ridge = cv.glmnet(x, y, alpha=0, type.measure = "mse", nfolds=10,grouped=FALSE)
plot(cv.ridge)
bestlam.ridge=cv.ridge$lambda.min #find the best tuning parameter

fit.ridge =glmnet(train.mat, df.train$SalePrice, alpha = 0, lambda = grid, thresh = 1e-12)
pred.ridge = predict (fit.ridge, s=bestlam.ridge, newx=test.mat)
mean(( pred.ridge - df.test$SalePrice)^2)

final.ridge=glmnet(x,y,alpha=0) #fit on the full data
ridge.coef=predict(final.ridge,type="coefficients",s=bestlam.ridge)[1:14,]
ridge.coef
ridge.coef[ridge.coef!=0] #contains all variables in our model

### Start modeling with the selected parameters ###

# First add the classification column
sp_modified <- rep(0, length(df$SalePrice))
sp_modified[which(df$SalePrice >= 200000)] <- 1

table(sp_modified)

df <- data.frame(df, sp_modified)
dim(df)
#remove SalePrice as we dont need for the model
#df = df[ , -which(names(df) %in% c("SalePrice"))]

set.seed(7736)
test = sample(n, round(n/4)) ## train indices are the rest
train = setdiff(1:n, test)

df.train = df[ train,]
df.test = df[test,]

# Logistic Regression: Model with all parameters
fit.glm <- glm(sp_modified ~ . - sp_modified - SalePrice, data = df, family = binomial, subset = train)
probs <- predict(fit.glm, df.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
conf_matrix = table(pred.glm, crim_modified.test)
conf_matrix
cm=confusionMatrix(data = pred.glm, reference = crim_modified.test)
cm$byClass
mean(pred.glm == crim_modified.test)

# Logistic Regression: Model with parameters selected by lasso
fit.glm <- glm(sp_modified ~ MS.SubClass + MS.Zoning + Lot.Area + Street + Lot.Shape + Land.Contour + Utilities + Lot.Config + Land.Slope + Neighborhood + Condition.1 + Condition.2 + Bldg.Type + House.Style + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + Roof.Style + Roof.Matl + Exterior.1st + Exterior.2nd + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual + Exter.Cond + Foundation + 
                 Bsmt.Qual + Bsmt.Exposure + BsmtFin.Type.1 + BsmtFin.SF.1 + BsmtFin.Type.2 + BsmtFin.SF.2 + Heating + X1st.Flr.SF + X2nd.Flr.SF + Bsmt.Full.Bath + Bsmt.Half.Bath + Full.Bath + Half.Bath + Bedroom.AbvGr + Kitchen.AbvGr + Kitchen.Qual + TotRms.AbvGrd + Functional + Fireplaces + Fireplace.Qu + Garage.Type + Garage.Yr.Blt + Garage.Finish + Garage.Cars + Garage.Area + Garage.Qual + Garage.Cond + Paved.Drive + Wood.Deck.SF + Open.Porch.SF + Enclosed.Porch + Screen.Porch + Pool.QC + Fence + Misc.Feature + Yr.Sold + Sale.Type + Sale.Condition + tot.sqft
               - SalePrice, data = df, family = binomial, subset = train)
#fit.glm <- glm(sp_modified ~ MS.SubClass -SalePrice
#               ,data = df, family = binomial, subset = train)

probs <- predict(fit.glm, Boston.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim_modified.test)
cm=confusionMatrix(data = pred.glm, reference = crim_modified.test)
cm$byClass
mean(pred.glm == crim_modified.test)

