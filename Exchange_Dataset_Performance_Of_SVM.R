library("e1071")
Ex_xls<-read.csv("Home/desktop/WU/Data Mining and Machine Learning")  # use your own folder 
summary(Ex_xls)

str(Ex_xls$`USD/EUR`)

x <- subset(Ex_xls, select = -`USD/EUR`)
y <-as.numeric(Ex_xls$`USD/EUR`)

Ex_xls_factor<-cbind(x, `USD/EUR`=as.factor(y))

str(Ex_xls_factor)

Ex_xlsTrain<-Ex_xls_factor[1:320,]
Ex_xlsTest<-Ex_xls_factor[321:390,]

x_factor <- subset(Ex_xlsTest, select = -`USD/EUR`)
y_factor <- Ex_xlsTest$`USD/EUR`

Ex_xls_svm <- svm(`USD/EUR` ~ ., data = Ex_xlsTrain)
summary(Ex_xls_svm)

Ex_xls_factor_predict <- predict(Ex_xls_svm, x_factor);
1-sum(Ex_xls_factor_predict == y_factor)/length(y_factor)

Ex_xls_svm_tuned <- tune(svm, `USD/EUR`~., data = Ex_xlsTrain,
                       ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
                       tunecontrol = tune.control(sampling = "cross"))

summary(Ex_xls_svm_tuned)

plot(Ex_xls_svm_tuned)

Ex_xls_svm_tuned$best.parameters

Ex_xls_svm <- svm(`USD/EUR` ~ ., data = Ex_xlsTrain, gamma = 0.07, cost = 1.5)
Ex_xls_factor_predict <- predict(Ex_xls_svm, x_factor);
1-sum(Ex_xls_factor_predict == y_factor)/length(y_factor)

Ex_xls_numeric<-cbind(x, `USD/EUR`=y)
str(Ex_xls_numeric)

Ex_xlsTrain<-Ex_xls_numeric[1:320,]
Ex_xlsTest<-Ex_xls_numeric[321:390,]

x_factor <- subset(Ex_xlsTest, select = -`USD/EUR`)
y_factor <- Ex_xlsTest$`USD/EUR`

Ex_xls_svm <- svm(`USD/EUR` ~ ., data = Ex_xlsTrain)
summary(Ex_xls_svm)

Ex_xls_factor_predict <- predict(Ex_xls_svm, x_factor);
sqrt( sum((Ex_xlsTest$`USD/EUR`-Ex_xls_factor_predict)^2))/length(Ex_xls_factor_predict)

Ex_xls_svm_tuned <- tune(svm, `USD/EUR`~., data = Ex_xlsTrain,
                       ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
                       tunecontrol = tune.control(sampling = "cross"))

summary(Ex_xls_svm_tuned)

plot(Ex_xls_svm_tuned)

Ex_xls_svm_tuned$best.parameters

Ex_xls_svm <- svm(`USD/EUR` ~ ., data = Ex_xlsTrain, gamma = 0.1, cost = 2)
Ex_xls_factor_predict <- predict(Ex_xls_svm, x_factor);
sqrt(sum((Ex_xlsTest$`USD/EUR`-Ex_xls_factor_predict)^2))/length(Ex_xls_factor_predict)

library(e1071)
x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
y=c(3,4,5,4,8,10,10,11,14,20,23,24,32,34,35,37,42,48,53,60)

