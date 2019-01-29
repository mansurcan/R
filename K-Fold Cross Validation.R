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

# load dataset
library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

summary(Whitewine)
str(Whitewine)
head(Whitewine)
dim(Whitewine)

anyNA(Whitewine)

Whitewine$quality <-as.factor(Whitewine$quality)

trctrl<-trainControl(method = "repeatedcv", number = 10,repeats = 3)

modelKFCV<-train(quality~.,data = Whitewine, trControl=trctrl, method="nb")

print(modelKFCV)
