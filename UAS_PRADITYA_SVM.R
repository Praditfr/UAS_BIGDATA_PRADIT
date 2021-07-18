library(e1071)
library(caret)
library(devtools)
data=read.csv("C:/Users/User/Documents/task/UAS_SVM_PFR.csv")
head(data)
dim(data)
n=round(nrow(data)*0.70)
n
set.seed(12345)
sample=sample(seq_len(nrow(data)), size = n)
train=data[sample, ]
dim(train)
test=data[-sample, ]
dim(test)
data.svm<-svm(factor(Pelunasan.Kredit) ~., data = train, probability=T)
head(data.svm)
prediks1 <- predict(data.svm, train, decision.values=T, probability=T)
head(prediks1)
confusionMatrix(factor(train$Pelunasan.Kredit), factor(prediks1))
prediks1 <- predict(data.svm, test, decision.values=T, probability=T)
head(prediks1)
confusionMatrix(factor(test$Pelunasan.Kredit), factor(prediks1))

