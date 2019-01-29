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
# Denormalize function

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

actualexchange <- Ex_xls[,3]
View(actualexchange)
actual_train <- actualexchange[4:320, ]
actual_test <- actualexchange[321:390, ]


# Create training model for 2 vs 3 inputs and 1 vs 2 hidden nodes
# Fit Neural network
# 2 lag variables

require(neuralnet)
set.seed=2
model1 <- neuralnet(USDEUR_norm~USDEUR_norm.lg1+USDEUR_norm.lg2, data=USDEUR_train, learningrate=0.2, hidden=1)
summary(model1)

plot(model1)
par(mar = numeric(4), family = 'serif')
plotnet(model1, alpha = 0.6)

install.packages("neuralnet")
library("neuralnet")
model_results<-neuralnet::compute(model1, USDEUR_test[-1])
print(model_results)
predicted_rate <- model_results$net.result
print(predicted_rate)

# Denormalize
predicted_rate_denom <- DEnormalize(predicted_rate)
#predicted_rate_denom_test = (predicted_rate * (max(Ex_xls$`USD/EUR`) - min(Ex_xls$`USD/EUR`)) + min(Ex_xls$`USD/EUR`))
#View(predicted_rate_denom)

#plot this, fix it (!)
plot(actual_test, predicted_rate_denom, col='blue', pch=16, ylab = "predicted exchange rates NN", xlab = "real exchange rates")


# Create training model for 2 vs 3 inputs and 1 vs 2 hidden nodes
# Fit Neural network
# 2 lag variables
require(neuralnet)
set.seed=2
model1 <- neuralnet(USDEUR_norm~USDEUR_norm.lg1+USDEUR_norm.lg2, data=USDEUR_train, learningrate=0.8, hidden=1)
summary(model1)

plot(model1)
par(mar = numeric(4), family = 'serif')
plotnet(model1, alpha = 0.6)

install.packages("neuralnet")
library("neuralnet")
model_results<-neuralnet::compute(model1, USDEUR_test[-1])
print(model_results)
predicted_rate <- model_results$net.result
print(predicted_rate)

# Denormalize
predicted_rate_denom <- DEnormalize(predicted_rate)
View(predicted_rate_denom)
#predicted_rate_denom_test = (predicted_rate * (max(Ex_xls$`USD/EUR`) - min(Ex_xls$`USD/EUR`)) + min(Ex_xls$`USD/EUR`))
#View(predicted_rate_denom)

#plot this, fix it (!)
plot(actual_test, predicted_rate_denom, col='blue', pch=16, ylab = "predicted exchange rates NN", xlab = "real exchange rates")



# Create training model for 2 vs 3 inputs and 1 vs 2 hidden nodes
# Fit Neural network
# 2 lag variables
require(neuralnet)
set.seed=2
model0 <- neuralnet(USDEUR_norm~USDEUR_norm.lg1+USDEUR_norm.lg2, data=USDEUR_train, learningrate=0.8, hidden=2)
summary(model1)

plot(model0)
par(mar = numeric(4), family = 'serif')
plotnet(model0, alpha = 0.6)

install.packages("neuralnet")
library("neuralnet")
model_results<-neuralnet::compute(model1, USDEUR_test[-1])
print(model_results)
predicted_rate <- model_results$net.result
print(predicted_rate)

# Denormalize
predicted_rate_denom <- DEnormalize(predicted_rate)
View(predicted_rate_denom)
#predicted_rate_denom_test = (predicted_rate * (max(Ex_xls$`USD/EUR`) - min(Ex_xls$`USD/EUR`)) + min(Ex_xls$`USD/EUR`))
#View(predicted_rate_denom)

#plot this, fix it (!)
plot(actual_test, predicted_rate_denom, xlab= 'x', ylab='y', col='blue', pch=16)

