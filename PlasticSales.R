library(forecast)
library(fpp)
library(smooth)

plastics <- read.csv('E:/excelr data/assignments/forcasting/PlasticSales.csv')
library(tseries)
new1 <-ts(plastics$Sales,frequency=12,start = c(49)) 
plot(new1)
new1 <- as.data.frame(new1)
train <- new[1:48]
test <- new[49:60]
train <- ts(train,frequency=12)
test <- ts(test,frequency=12)

#Holt Winter's function
hw1 <- HoltWinters(train,alpha = 0.2,gamma = F,beta = F)
hw1_pred <- forecast(hw1)
hw1_pred <- data.frame(predict(hw1,n.ahead=12))
plot(forecast(hw1,h=12))
MAPE1 <- MAPE(hw1_pred$fit,test)*100
MAPE1

##Holt Winter's function(with Beta)
hw2 <- HoltWinters(train,alpha = 0.2,gamma = F,beta = 0.1)
hw2_pred <-  forecast(hw2)
hw2_pred <- data.frame(predict(hw2,n.ahead=12))
plot(forecast(hw2,h=12))
MAPE2 <- MAPE(hw2_pred$fit,test)*100
MAPE2

#Without optimul values
hw3 <- HoltWinters(train,gamma = F,beta =F)
hw3_pred <-  forecast(hw3)
hw3_pred <- data.frame(predict(hw3,n.ahead=12))
plot(forecast(hw3,h=12))
MAPE3 <- MAPE(hw3_pred$fit,test)*100
MAPE3

#simple exponential function

ses1 <- ses(train,alpha=0.2)
ses1_pred <- data.frame(predict(ses1,n.ahead=12))
plot(forecast(ses1,h=12))
MAPE4 <- MAPE(ses1_pred$Point.Forecast,test)*100
MAPE4