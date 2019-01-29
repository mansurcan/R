#Part 3

install.packages("caret")
library(caret)
installed.packages("rpart.plot")
library(rpart.plot)
install.packages("rpart")
library(rpart)
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)

library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

summary(Whitewine)
str(Whitewine)
head(Whitewine)

plot(as.factor(Whitewine$quality), col=rainbow(10))
box()

Whitewine$qual3code <- mapvalues(Whitewine$quality, 
                                 from = c("3", "4", "5", "6", "7", "8", "9"), 
                                 to = c("1", "1", "2", "3", "4", "4", "4"))
Whitewine$qual3code<- factor(Whitewine$qual3code, levels=c("1", "2", "3","4"), 
                             labels= c("low",  "medium",  "high", "veryhigh")) 

plot(as.factor(Whitewine$qual3code), col=rainbow(10))
box()
summary(Whitewine$qual3code)

n.whitewine <-cbind(Whitewine[,1:11],Whitewine[,13])
n.whitewine$qual3code <-as.factor(n.whitewine$qual3code)
dim(n.whitewine)

n.whitewine$qual3code<-as.factor(n.whitewine$qual3code)

#Data Slicing
set.seed(3033)
intrain <- createDataPartition(y = n.whitewine$qual3code, p= 0.67, list = FALSE)
training <- n.whitewine[intrain,]
testing <- n.whitewine[-intrain,]

#check dimensions of train & test set
dim(training);dim(testing)

#Preprocessing and Training
#Is there any missing value in data set?
anyNA(n.whitewine)
summary(n.whitewine)

#Training the Decision Tree classifier with criterion as information gain
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(qual3code ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

#Trained Decision Tree classifier results
dtree_fit

#Plot Decision Tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

#Prediction
testing[1,]
predict(dtree_fit, newdata = testing[1,])

test_pred<-predict(dtree_fit,newdata = testing)
print(test_pred)

str(test_pred)
str(testing$qual3code)

confusionMatrix(test_pred,testing$qual3code) #check accuracy error

#Training the Decision Tree classifier with criterion as gini index
set.seed(3333)
dtree_fit_gini <- train(qual3code ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$qual3code )  #check accuracy error



