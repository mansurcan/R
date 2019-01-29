install.packages("caret")
library(caret)
installed.packages("rpart.plot")
library(rpart.plot)
install.packages("rpart")
library(rpart)

library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

summary(Whitewine)
str(Whitewine)
head(Whitewine)

set.seed(3033)

##### Whitewine$`fixed acidity` : This should be first column at data set. Its written wrong on Tutorial word document.

intrain <- createDataPartition(y = Whitewine$`fixed acidity`, p= 0.67, list = FALSE)
training <- Whitewine[intrain,]
testing <- Whitewine[-intrain,]

#check dimensions of train & test set
dim(training);dim(testing)

#Preprocessing and Training
#Is there any missing value in data set?
anyNA(Whitewine)
summary(Whitewine)

#Training the Decision Tree classifier with criterion as information gain
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)

### This code gives error: Error in `[.data.frame`(m, labs) : undefined columns selected

dtree_fit <- train(quality ~., data = training, method = "rpart",
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
confusionMatrix(test_pred,testing$quality) #check accuracy 

