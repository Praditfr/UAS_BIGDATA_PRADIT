library(readr)
str(UAS_PCA_MLR)
UAS_PCA_MLR$Saving <- (UAS_PCA_MLR$Saving - min(UAS_PCA_MLR$Saving))/(max(UAS_PCA_MLR$Saving) - min(UAS_PCA_MLR$Saving))
UAS_PCA_MLR$Deposit <- (UAS_PCA_MLR$Deposit - min(UAS_PCA_MLR$Deposit))/(max(UAS_PCA_MLR$Deposit) - min(UAS_PCA_MLR$Deposit))
UAS_PCA_MLR$KK <- (UAS_PCA_MLR$KK - min(UAS_PCA_MLR$KK))/(max(UAS_PCA_MLR$KK) - min(UAS_PCA_MLR$KK))
UAS_PCA_MLR$Tab_Bisnis <- (UAS_PCA_MLR$Tab_Bisnis - min(UAS_PCA_MLR$Tab_Bisnis))/(max(UAS_PCA_MLR$Tab_Bisnis) - min(UAS_PCA_MLR$Tab_Bisnis))
UAS_PCA_MLR$Limit_Kredit_Mortgage <- (UAS_PCA_MLR$Limit_Kredit_Mortgage - min(UAS_PCA_MLR$Limit_Kredit_Mortgage))/(max(UAS_PCA_MLR$Limit_Kredit_Mortgage) - min(UAS_PCA_MLR$Limit_Kredit_Mortgage))
UAS_PCA_MLR.raw = UAS_PCA_MLR
dim(UAS_PCA_MLR.raw)
length(UAS_PCA_MLR.raw$Product_holding)
UAS_PCA_MLR.raw[UAS_PCA_MLR.raw==""] <- NA
sapply(UAS_PCA_MLR.raw, function(x) sum(is.na(x)))
colSums(is.na(UAS_PCA_MLR.raw))
sapply(UAS_PCA_MLR.raw, function(x) length(unique(x)))
UAS_PCA_MLR$Product_holding <- as.factor(UAS_PCA_MLR$Product_holding)
set.seed(700)
ind <- sample(2, nrow(UAS_PCA_MLR), replace = TRUE, prob = c(0.7, 0.3))
training <- UAS_PCA_MLR[ind==1,]
testing <- UAS_PCA_MLR[ind==2,]
library(nnet)
training$Product_holding <- relevel(training$Product_holding, ref="1")
dit <- multinom(Product_holding~., data = training)
summary(dit)
z <- summary(dit)$coefficients/summary(dit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
p <- predict(dit, training)
head(p)
head(training$Product_holding)
p <- predict(dit, training)
tab <- table(p, training$Product_holding)
tab
1- sum(diag(tab))/sum(tab)
p1 <- predict(dit, testing)
tab1 <- table(p1, testing$Product_holding)
tab1
1 - sum(diag(tab1))/sum(tab1)
table(training$Product_holding)
n <- table(training$Product_holding)
n/sum(n)
tab/colSums(tab)
tab1/colSums(tab1)
