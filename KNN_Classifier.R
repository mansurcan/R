
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

#Normalize
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

#Normalize Whitewine data
Whitewine_norm<-as.data.frame(lapply(Whitewine[,c(1,2,3,4,5,6,7,8,9,10,11)], normalize))
summary(Whitewine_norm)

#Training and Test Sets
set.seed(1234)
ind <- sample(2, nrow(Whitewine), replace=TRUE, prob=c(0.67, 0.33))

# Compose training set
Whitewine.training <- Whitewine[ind==1, 1:11]

# Inspect training set
head(Whitewine.training)

# Compose test set
Whitewine.test <- Whitewine[ind==2, 1:11]

# Inspect test set
head(Whitewine.test)

# Compose `Whitewine` training labels
Whitewine.trainLabels <- Whitewine[ind==1,12]

# Inspect result
print(Whitewine.trainLabels)

# Compose `Whitewine` test labels
Whitewine.testLabels <- Whitewine[ind==2, 12]

# Inspect result
print(Whitewine.testLabels)

#TRY this:
cl = Whitewine.trainLabels[,1]
knn(Whitewine.training, Whitewine.test, cl, k = 3)
dim(Whitewine.training)
dim(Whitewine.test)
length(cl)

#Building Your KNN Classifier
# Build the model
Whitewine_pred <- knn(train = Whitewine.training, test = Whitewine.test, cl = Whitewine.trainLabels, k=3)
# Inspect `iris_pred`
Whitewine_pred




summary(Whitewine)
str(Whitewine)
head(Whitewine)
data.frame(Whitewine)
data.frame
make.names(Whitewine)

Whitewine$quality <-as.factor(Whitewine$quality)

set.seed(1234)

#Create index to split based on labels  
index <- createDataPartition(y = Whitewine$quality, p=0.67, list=FALSE)
# Subset training set with index
Whitewine.training <- Whitewine[index,]
# Subset test set with index
Whitewine.test <- Whitewine[-index,]

anyNA(Whitewine)

Whitewine.training[["quality"]] = factor(Whitewine.training[["quality"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(quality ~., data = Whitewine.training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

test_pred <- predict(knn_fit, newdata = Whitewine.test)
test_pred

confusionMatrix(test_pred, Whitewine.test$quality)

# Train a model
model_knn <- train(Whitewine.training[, 1:11], Whitewine.training[, 12], method='knn')

# Predict the labels of the test set
predictions<-predict.train(object=model_knn,Whitewine.test[,1:11], type="raw")
# Evaluate the predictions
table(predictions)
# Confusion matrix 
confusionMatrix(predictions,Whitewine.test[,12])


