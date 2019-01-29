install.packages("caret")
library(caret)
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
#installed.packages("rpart.plot")
#library(rpart.plot)
install.packages("rpart")
library(rpart)
install.packages("klaR")
library("klaR")
install.packages("MASS")
library(MASS)
install.packages("magrittr")
library(magrittr)
install.packages("class")
library(class)

library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

plot(as.factor(Whitewine$quality), col=rainbow(10))
box()
summary(Whitewine$quality)

Whitewine$qual3code <- mapvalues(Whitewine$quality, 
                                 from = c("3", "4", "5", "6", "7", "8", "9"), 
                                 to = c("1", "1", "1", "2", "3", "3", "3"))
Whitewine$qual3code<- factor(Whitewine$qual3code, levels=c("1", "2", "3"), 
                             labels= c("low",  "medium",  "high")) 

plot(as.factor(Whitewine$qual3code), col=rainbow(10))
box()
summary(Whitewine$qual3code)

n.whitewine <-cbind(Whitewine[,1:11],Whitewine[,13])
n.whitewine$qual3code <-as.factor(n.whitewine$qual3code)
dim(n.whitewine)

n.whitewine$qual3code<-as.factor(n.whitewine$qual3code)

index <- createDataPartition(n.whitewine$qual3code, p=0.67, list=FALSE)

n.whitewine.training <- n.whitewine[index,]

n.whitewine.test <- n.whitewine[-index,]

model_knn <- train(n.whitewine.training[, 1:11], n.whitewine.training[, 12], method='knn')

predictions<-predict.train(object=model_knn,n.whitewine.test[,1:11], type="raw")

table(predictions)

confusionMatrix(predictions,n.whitewine.test[,12])


