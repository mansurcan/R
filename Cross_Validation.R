#Part 2
install.packages(c("caret", "MASS","klaR", "e1071", "lattice","ggplot2"), dependencies = TRUE)
install.packages("plyr")
install.packages("MASS")

library("lattice")
library("ggplot2")
library("caret")
library("klaR")
library("e1071")
library("MASS")
library("plyr")

require(caret)
require(klaR)
require(e1071)
require(lattice)
require(ggplot2)
require(MASS)
require(plyr)

# load dataset
library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

summary(Whitewine)
str(Whitewine)
head(Whitewine)
dim(Whitewine)

plot(as.factor(Whitewine$quality), col=rainbow(10))
box()
summary(Whitewine$quality)

Whitewine$Qualitycode <- mapvalues(Whitewine$quality, 
                                 from = c("3", "4", "5", "6", "7", "8", "9"), 
                                 to = c("1", "1", "1", "2", "3", "3", "3"))
Whitewine$Qualitycode<- factor(Whitewine$Qualitycode, levels=c("1", "2", "3"), 
                             labels= c("low",  "medium",  "high")) 

plot(as.factor(Whitewine$Qualitycode), col=rainbow(10))
box()
summary(Whitewine$Qualitycode)

n.whitewine <-cbind(Whitewine[,1:11],Whitewine[,13])
n.whitewine$Qualitycode <-as.factor(n.whitewine$Qualitycode)
dim(n.whitewine)

n.whitewine$Qualitycode<-as.factor(n.whitewine$Qualitycode)

intrain<-createDataPartition(y=n.whitewine$Qualitycode,p=0.67,list = FALSE)
training <- n.whitewine[intrain,]
testing <- n.whitewine[-intrain,]

dim(training)
dim(testing)
anyNA(n.whitewine)
summary(n.whitewine)

#hold out method
modelHO<-NaiveBayes(Qualitycode~.,data = training)
#make predictions
x_test<-testing[,1:11]
y_test<-testing[,12]
predictions<-predict(modelHO,x_test)
confusionMatrix(predictions$class, y_test)

#ROC
install.packages("pROC")
library(pROC)
require(pROC)
install.packages("AUC")
library(AUC)
require(AUC)
install.packages("ROCR")
library(ROCR)
require(ROCR)
install.packages("klaR")
library(klaR)
require(klaR)
install.packages("e1071")
library(e1071)
require(e1071)
install.packages("caret")
library(caret)
require(caret)
install.packages("rattle") #rgtk2
library(rattle)
require(rattle)
install.packages("randomForest")
library(randomForest)
require(randomForest)
install.packages("class")
library(class)
require(class)
install.packages("boot")
library(boot)
require(boot)
install.packages("ISLR")
library(ISLR)
require(ISLR)
install.packages("caTools")
library(caTools)
require(caTools)


lvls = levels(n.whitewine$Qualitycode)

#aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')

for (type.id in 1:3) {
  type = as.factor(training$Qualitycode == lvls[type.id])
  
  nbmodel = NaiveBayes(Qualitycode~.,data = training[,-12])

  print(nbmodel)
  nbprediction = predict(nbmodel, testing[,-12], type='raw')
  
  score = nbprediction$posterior[, 'TRUE']
  actual.class = testing$Qualitycode == lvls[type.id]
  
  pred = prediction(score, actual.class)
  nbperf = performance(pred, "tpr", "fpr")
  
  roc.x = unlist(nbperf@x.values)
  roc.y = unlist(nbperf@y.values)
  lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
  
  nbauc = performance(pred, "auc")
  nbauc = unlist(slot(nbauc, "y.values"))
  aucs[type.id] = nbauc
}

lines(x=c(0,1), c(0,1))

mean(aucs)

#Cross Validation
require(caret)
require(klaR)
train_control<-trainControl(method="cv", number=10)
grid<-expand.grid(.fL=c(0), .usekernel=c(FALSE),.adjust=c(FALSE))
modelCrossValidation<-train(Qualitycode~., data=n.whitewine, trControl=train_control, method="nb", tuneGrid=grid)
print(modelCrossValidation)

#make predictions
CVx_test<-n.whitewine[,1:11]
CVy_test<-n.whitewine[,12]
predictionsCV<-predict(modelCrossValidation,CVx_test)
print(predictionsCV)
confusionMatrix(predictionsCV, CVy_test)

#Repeated k-fold Cross Validation
train_control<-trainControl(method="repeatedcv", number=10, repeats = 3)
modelRepeatedCrossValidation<-train(Qualitycode~., data=n.whitewine, trControl=train_control, method="nb")
print(modelRepeatedCrossValidation)

#make predictions
RCVx_test<-n.whitewine[,1:11]
RCVy_test<-n.whitewine[,12]
predictionsRCV<-predict(modelRepeatedCrossValidation,RCVx_test)
print(predictionsRCV)
confusionMatrix(predictionsRCV, RCVy_test)

#Leave One Out Cross Validation
train_control<-trainControl(method="LOOCV")
modelLOOCV<-train(Qualitycode~., data=n.whitewine, trControl=train_control, method="nb")
print(modelLOOCV)

#make predictions
LOOCVx_test<-n.whitewine[,1:11]
LOOCVy_test<-n.whitewine[,12]
predictionsLOOCV<-predict(modelLOOCV,LOOCVx_test)
print(predictionsLOOCV)
confusionMatrix(predictionsLOOCV, LOOCVy_test)


