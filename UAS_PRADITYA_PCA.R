library(readr)
str(UAS_PCA_MLR)
UAS_PCA_MLR$Saving <- (UAS_PCA_MLR$Saving - min(UAS_PCA_MLR$Saving))/(max(UAS_PCA_MLR$Saving) - min(UAS_PCA_MLR$Saving))
UAS_PCA_MLR$Deposit <- (UAS_PCA_MLR$Deposit - min(UAS_PCA_MLR$Deposit))/(max(UAS_PCA_MLR$Deposit) - min(UAS_PCA_MLR$Deposit))
UAS_PCA_MLR$KK <- (UAS_PCA_MLR$KK - min(UAS_PCA_MLR$KK))/(max(UAS_PCA_MLR$KK) - min(UAS_PCA_MLR$KK))
UAS_PCA_MLR$Tab_Bisnis <- (UAS_PCA_MLR$Tab_Bisnis - min(UAS_PCA_MLR$Tab_Bisnis))/(max(UAS_PCA_MLR$Tab_Bisnis) - min(UAS_PCA_MLR$Tab_Bisnis))
UAS_PCA_MLRLimit_Kredit_Mortgage <- (UAS_PCA_MLR$Limit_Kredit_Mortgage - min(UAS_PCA_MLR$Limit_Kredit_Mortgage))/(max(UAS_PCA_MLR$Limit_Kredit_Mortgage) - min(UAS_PCA_MLR$Limit_Kredit_Mortgage))
head(UAS_PCA_MLR)
summary(UAS_PCA_MLR)
myPr <- prcomp(UAS_PCA_MLR[, 7:13])
prcomp(~Income + Limit_Kredit_Mortgage, data = UAS_PCA_MLR)
plot (UAS_PCA_MLR$Income, UAS_PCA_MLR$Limit_Kredit_Mortgage)
plot (scale(UAS_PCA_MLR$Income), scale(UAS_PCA_MLR$Limit_Kredit_Mortgage))
myPr
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0)