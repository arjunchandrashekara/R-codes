getwd()
library(readxl)
airlines <- read_excel("E:/excelr data/assignments/forcasting/Airlines+Data.xlsx")
X <- data.frame(outer(rep(month.abb,length=96),month.abb,'==')+0)
colnames(X) <- month.abb
air_new <- cbind(airlines,X)
air_new['t'] <- c(1:96)
air_new['t_square'] <- air_new['t']*air_new['t']
air_new['log_t'] <- log(air_new['Passengers'])
attach(air_new)
train <- air_new[1:80,]
test <- air_new[81:96,]

#linear model
model1 <- lm(Passengers~t,data=train)
pred_model1 <- data.frame(predict(model1,interval='predict',newdata=test))
rmse_model1 <- sqrt(mean((air_new$Passengers-pred_model1$fit)^2,na.rm = T))
#exp_model
model2 <- lm(log_t~t,data=train)
pred_model2 <- data.frame(predict(model2,interval='predict',newdata=test))
rmse_model2 <- sqrt(mean((air_new$Passengers-pred_model2$fit)^2,na.rm = T))
#quadratic model
model3 <- lm(Passengers~t+t_square,data=train)
pred_model3 <- data.frame(predict(model3,interval='predict',newdata=test))
rmse_model3 <- sqrt(mean((air_new$Passengers-pred_model3$fit)^2,na.rm = T))
#additive seasonality
model4 <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
pred_model4 <- data.frame(predict(model4,interval='predict',newdata=test))
rmse_model4 <- sqrt(mean((air_new$Passengers-pred_model4$fit)^2,na.rm = T))
#additive seasonlity with quadratic
model5 <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
pred_model5 <- data.frame(predict(model5,interval='predict',newdata=test))
rmse_model5 <- sqrt(mean((air_new$Passengers-pred_model5$fit)^2,na.rm = T))
#multiplicative seasonality
model6 <- lm(log_t~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
pred_model6 <- data.frame(predict(model6,interval='predict',newdata=test))
rmse_model6 <- sqrt(mean((air_new$Passengers-pred_model6$fit)^2,na.rm = T))
#multiplicative additional seasonality
model7 <- lm(log_t~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
pred_model7 <- data.frame(predict(model7,interval='predict',newdata=test))
rmse_model7 <- sqrt(mean((air_new$Passengers-pred_model7$fit)^2,na.rm = T))

rmse_data <- data.frame(type=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),rmse=c(rmse_model1,rmse_model2,rmse_model3,rmse_model4,rmse_model5,rmse_model6,rmse_model7))
View(rmse_data)
write.csv(rmse_data,file='rmse_airlines')

new <- cbind(airlines,pred_model4)
write.csv(new,file='airlines')
