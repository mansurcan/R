install.packages("neuralnet")
library("neuralnet")
install.packages("NeuralNetTools")
library("NeuralNetTools")
install.packages("plyr")
library("plyr")
install.packages("zoo")
library("zoo")
require(zoo)
install.packages("plm")
library("plm")
install.packages("data.table")
library("data.table")
install.packages("RSNNS")
library("RSNNS")
install.packages("DataCombine")
library("DataCombine")
install.packages("DataCombine")
library("DataCombine")
install.packages("tidyquant")
library("tidyquant")
install.packages("tidyr")
library("tidyr")
install.packages("timetk")
library("timetk")
install.packages("stringr")
library("stringr")
install.packages("dplyr")
library("dplyr")
install.packages("knitr")
library("knitr")
install.packages("xts")
library("xts")
install.packages("TTR")
library("TTR")
install.packages("MASS")
library("MASS")
install.packages("grid")
library("grid")
install.packages("TTR")
library("TTR")
install.packages("Metrics")
library("Metrics")

#functions for calculating NN Model Fit (Step 4)
rmse2 <- function(act, pred)
{
  sqrt(mean((act-pred)^2))
}

mse <- function(act, pred)
{
  (mean((act-pred)^2))
}

mape <- function(act,pred){
  mape <- mean(abs((act - pred)/act))*100
  return (mape)
}

# Denormalize
# DEnormalize function

DEnormalize <- function(y) {
  return((y*(b-a)+a))
}

# normalize function
summary(Ex_xls$`USD/EUR`)
a=min(Ex_xls$`USD/EUR`)
b=max(Ex_xls$`USD/EUR`)

normalize <- function(x) {
  return((x - a) / (b-a))
}

Ex_xls$USDEUR_norm <- normalize(Ex_xls$`USD/EUR`)


# Lagging Time Series Data t-1 t-2 t-3

Ex_xls$USDEUR_norm.lg1<-lag(usx,1,na.pad=TRUE)
Ex_xls$USDEUR_norm.lg2<-lag(usx,2,na.pad=TRUE)
Ex_xls$USDEUR_norm.lg3<-lag(usx,3,na.pad=TRUE)

# Training a model on the  Data

newexchange <- Ex_xls[,5:8]
USDEUR_train <- newexchange[4:320, ]
USDEUR_test <- newexchange[321:390, ]



#This is for Objective 4 Utilizing SVR (regression)
# Create training model for 2 vs 3 inputs and 1 vs 2 hidden nodes
# Fit SVR network

#Model0 

library(e1071)
require(e1071)
require(caret)
require(Metrics)

#functions for calculating NN Model Fit (Step 4)
rmse2 <- function(act, pred)
{
  sqrt(mean((act-pred)^2))
}

mse <- function(act, pred)
{
  (mean((act-pred)^2))
}

mape <- function(act,pred){
  mape <- mean(abs((act - pred)/act))*100
  return (mape)
}

#Running SVR Models

model0 <- svm(USDEUR_norm~., data=USDEUR_train)
print(model0)
plot(model0)
summary(model0) #check the details of SVR model

predicted_rate<-predict(model0, USDEUR_train[-1])

svm_tune <- tune(svm, USDEUR_norm ~ ., data = USDEUR_train, ranges=list(epsilon=seq(0,1,0.01), cost=10^2))
print(svm_tune)
summary(svm_tune)
plot(svm_tune) 
print(svm_tune$best.parameters)

#the best model
best_mod <- svm_tune$best.model
View(best_mod)
print(best_mod)
best_mod_pred <- predict(best_mod, USDEUR_train) 
error_best_mod <- (USDEUR_train$USDEUR_norm - best_mod_pred)

# Denormalize
predicted_rate_denom0 <- DEnormalize(predicted_rate)
View(predicted_rate_denom0)

actualexchange <- Ex_xls[,3]
View(actualexchange)
actual_train <- actualexchange[4:320, ]
actual_test <- actualexchange[321:390, ]

#Model Fit/Performance

act= actualexchange# Actual usd/eur exchange rate
pred= predicted_rate_denom0 # Predicted usd/eur exchange rate (denormalized)

# Calculate Root Mean Square Error (RMSE)
best_mod_RMSE <- rmse(actual_train, predicted_rate_denom0) 
print(best_mod_RMSE)
# Calculate Mean Square Error (MSE)
best_mod_MSE <- mse(actual_train, predicted_rate_denom0)
print(best_mod_MSE)
# Calculate Mean Absolute Percentage Error (MAPE)
best_mod_mape <- mape(actual_train, predicted_rate_denom0)
print(best_mod_mape)
# Calculate cORRELATION (COR)
best_mod_cor <- cor(actual_train, predicted_rate_denom0)
print(best_mod_cor)

plot(svm_tune)

#Model1

library(e1071)
require(e1071)
require(caret)

model1 <- svm(USDEUR_norm~USDEUR_norm.lg1+USDEUR_norm.lg2, data=USDEUR_train)
print(model1)
plot(model1)
summary(model1) #check the details of SVR model

predicted_rate<-predict(model1, USDEUR_train[-1])

svm_tune <- tune(svm, USDEUR_norm ~ USDEUR_norm.lg1+USDEUR_norm.lg2, data = USDEUR_train, ranges=list(epsilon=seq(0,1,0.01), cost=10^2))
print(svm_tune$best.parameters)
summary(svm_tune)
plot(svm_tune) #doesnt work
print(svm_tune$best.parameters)

#the best model
best_mod <- svm_tune$best.model
View(best_mod)
print(best_mod)
best_mod_pred <- predict(best_mod, USDEUR_train) 
error_best_mod <- (USDEUR_train$USDEUR_norm - best_mod_pred)

# Denormalize
predicted_rate_denom1 <- DEnormalize(predicted_rate)
View(predicted_rate_denom1)

actualexchange <- Ex_xls[,3]
View(actualexchange)
actual_train <- actualexchange[4:320, ]
actual_test <- actualexchange[321:390, ]

#Model Fit/Performance

act= actualexchange# Actual usd/eur exchange rate
pred= predicted_rate_denom1 # Predicted usd/eur exchange rate (denormalized)

# Calculate Root Mean Square Error (RMSE)
best_mod_RMSE <- rmse(actual_train, predicted_rate_denom1) 
print(best_mod_RMSE)
# Calculate Mean Square Error (MSE)
best_mod_MSE <- mse(actual_train, predicted_rate_denom1)
print(best_mod_MSE)
# Calculate Mean Absolute Percentage Error (MAPE)
best_mod_mape <- mape(actual_train, predicted_rate_denom1)
print(best_mod_mape)
# Calculate cORRELATION (COR)
best_mod_cor <- cor(actual_train, predicted_rate_denom1)
print(best_mod_cor)

plot(svm_tune)
