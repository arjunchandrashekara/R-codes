library(forecast)
library(fpp)
library(smooth)

plastic_bags <- read.csv(file.choose())
class(plastic_bags)
library(tseries)
plastic_bags_ts <- ts(plastic_bags$Sales,frequency = 12,start = c(49))
View(plastic_bags_ts)
train <- plastic_bags_ts[1:48]
test <- plastic_bags_ts[49:60]
train <- ts(train,frequency=12)
test <- ts(test,frequency=12)
plot(plastic_bags_ts)

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2

hw_a <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a_pred <- data.frame(predict(hw_a,n.ahead=12))
plot(forecast(hw_a,h=12))
MAPE_a <- MAPE(hw_a_pred$fit,test)*100
MAPE_a

# with alpha = 0.2, beta = 0.1
hw_b <- HoltWinters(train,alpha=0.2,beta=0.1,gamma=F)
hw_b_pred <- data.frame(predict(hw_b,n.ahead = 12))
plot(forecast(hw_b,h=12))
MAPE_b <- MAPE(hw_b_pred$fit,test)*100
MAPE_b

# with alpha = 0.2, beta = 0.1 and gamma=0.1
hw_c <- HoltWinters(train,alpha=0.2,beta=0.1,gamma=0.1)
hw_c_pred <- data.frame(predict(hw_c,n.ahead = 12))
plot(forecast(hw_c,h=12))
MAPE_c <- MAPE(hw_c_pred$fit,test)*100
MAPE_c

# without the optimul values
hw_d <- HoltWinters(train,beta=F,gamma=F)
hw_d_pred <- data.frame(predict(hw_d,n.ahead = 12))
plot(forecast(hw_d,h=12))
MAPE_d <- MAPE(hw_d_pred$fit,test)*100
MAPE_d

#without alpha,beta,gamma
hw_e <- HoltWinters(train)
hw_e_pred <- data.frame(predict(hw_e,n.ahead = 12))
plot(forecast(hw_e,h=12))
MAPE_e <- MAPE(hw_e_pred$fit,test)*100
MAPE_e
# outcome of HoltWInters function
final_outcome <- data.frame(type=c('hw_a','hw_b','hw_c','hw_d','hw_e'),Mape=c(MAPE_a,MAPE_b,MAPE_c,MAPE_d,MAPE_e))
getwd()
setwd( "E:/excelr data/assignments/forcasting")
write.csv(final_outcome,file='holtwinters')

#Movind average
ma_model1<-sma(train)
ma_pred<-data.frame(predict(ma_model1,h=12))
ma_pred
plot(forecast(ma_model1))
ma_mape<-MAPE(ma_pred$Point.Forecast,test)*100
ma_mape