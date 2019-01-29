#Part 2 : Formation of Training and Test Sets

install.packages(c("caret", "MASS","klaR", "e1071", "lattice","ggplot2"), dependencies = TRUE)

library("lattice")
library("ggplot2")
library("caret")
library("klaR")
library("e1071")
library("MASS")

require(caret)
require(klaR)
require(e1071)
require(lattice)
require(ggplot2)
require(MASS)

library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

summary(Whitewine)
str(Whitewine)
head(Whitewine)
dim(Whitewine)

Whitewine$quality <-as.factor(Whitewine$quality)

# define an 2/3 and 1/3 train/test split of the dataset
split=0.67
trainIndex <- createDataPartition(Whitewine$quality, p=split, list=FALSE)
data_train <- Whitewine[ trainIndex,]
data_test <- Whitewine[-trainIndex,]

data_train[["quality"]] = factor(data_train[["quality"]])

#4th Task: Build Train and Test a Naïve Bayes type Classifier
# train a naive bayes model
model <- NaiveBayes(quality~., data=data_train)
# make predictions
x_test <- data_test[,1:11]
y_test <- data_test[,12]

predictions <- predict(model, x_test$)

# summarize results
confusionMatrix(predictions$class, y_test)

