

#CTRL + L : clear console
#rm(list = ls()) : will remove all in global environment

#Part 1

install.packages(c("visualize", "ggplot2", "MASS", "lattice",
                   "RColorBrewer",  "Caret", "corrgram", "gplots",
                   "RGtk2", "rattle", "arules", "arulesViz", "GGally",
                   "car"))
install.packages("visualize")
install.packages("ggplot2")
install.packages("MASS")
install.packages("lattice")
install.packages("RColorBrewer")
install.packages("caret")
install.packages("corrgram")
install.packages("gplots")
install.packages("RGtk2")
install.packages("rattle")
install.packages("arules")
install.packages("arulesViz")
install.packages("GGally")
install.packages("car")

required.packages <- c("visualize", "ggplot2", "MASS", "lattice",
                       "RColorBrewer",  "Caret", "corrgram", "gplots", "car",
                       "RGtk2", "rattle", "arules", "arulesViz", "GGally")

library(visualize)
library(ggplot2)
library(MASS)
library(lattice)
library(RColorBrewer)
library(caret)
library(corrgram)
library(gplots)
library(rattle)
library(arules)
library(arulesViz)
library(GGally)
library(car)

library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
View(Whitewine)

summary(Whitewine)
str(Whitewine)
head(Whitewine)

plot(Whitewine, method="graph", interactive=TRUE, shading=NA)

#Part 2 : Formation of Training and Test Sets

# Model Evaluation via caret

# Install and load caret and klaR (NB) packages
install.packages(c("caret", "klaR", "e1071"), dependencies = TRUE)
install.packages("lattice")
library("lattice")
install.packages("ggplot2")
library("ggplot2")
library("caret")
library("klaR")
library("e1071")
require(caret)
require(klaR)
require(e1071)
require(lattice)
require(ggplot2)

Whitewine$quality <-as.factor(Whitewine$quality)

# Data Split

split=2/3
trainIndex <- createDataPartition(Whitewine$quality, p = split, list = FALSE)
data_train <- Whitewine[ trainIndex, ]
data_test  <- Whitewine[-trainIndex, ]

# train a naive bayes model
model <- NaiveBayes(quality~., data=data_train)

# make predictions
x_test <- data_test[,1:11]
y_test <- data_test[,12]
predictions <- predict(model, x_test)

# summarize results
confusionMatrix(predictions$class, y_test)
