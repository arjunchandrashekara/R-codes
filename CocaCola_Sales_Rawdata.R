setwd('E:/excelr data/assignments/forcasting')
library(readxl)
cocacola <- read_excel('E:/excelr data/assignments/forcasting/CocaCola_Sales_Rawdata.xlsx')
x <- c(1:4)
X <- data.frame(outer(rep(x,length=42),x,'==')+0)
View(X)
colnames(X) <- c('Q1','Q2','Q3','Q4')
cocacola_new <- cbind(cocacola,X)
cocacola_new['t'] <- c(1:42)
cocacola_new['log_t'] <- log(cocacola_new['Sales'])
cocacola_new['t_square'] <- cocacola_new['t']*cocacola_new['t']
attach(cocacola_new)
train <- cocacola_new[1:35,]
test <- cocacola_new[36:42,]


#linear Model
linear_model <- lm(Sales~t,data=train)
summary(linear_model)
pred_lm <- data.frame(predict(linear_model,interval='predict',newdata = test))
rmse_pred_lm <- sqrt(mean((cocacola_new$Sales-pred_lm$fit)^2,na.rm = T))
rmse_pred_lm

# exponemtial_model
exp_model <- lm(log(Sales)~t,data=cocacola_new)
summary(exp_model)
pred_exp_model <- data.frame(predict(exp_model,interval='predict',newdata = test))
rmse_pred_em <- sqrt(mean((cocacola_new$Sales-pred_exp_model$fit)^2,na.rm=T))
rmse_pred_em

#Quadratic model
Quad_model <- lm(Sales~t+t_square,data=cocacola_new)
summary(Quad_model)
pred_Quad_model <- data.frame(predict(Quad_model,interval = 'predict',newdata = test))
rmse_Quad_model <- sqrt(mean((cocacola_new$Sales-pred_Quad_model$fit)^2,na.rm = T))
rmse_Quad_model

#Additional Seasonabilty
add_sea_model <- lm(Sales~Q1+Q2+Q3+Q4,data=cocacola_new)
summary(add_sea_model)
pred_add_sea_model <- data.frame(predict(add_sea_model,interval="predict",newdata = test))
rmse_add_sea_model <- sqrt(mean((cocacola_new$Sales-pred_add_sea_model$fit)^2,na.rm = T))
rmse_add_sea_model

# additional Seasonality with Quadratic
add_sea_model_Quad <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cocacola_new)
summary(add_sea_model_Quad)
pred_add_sea_model_Quad <- data.frame(predict(add_sea_model_Quad,interval = 'predict',newdata = test))
rmse_add_sea_model_Quad <- sqrt(mean((cocacola_new$Sales-pred_add_sea_model_Quad$fit)^2,na.rm = T))
rmse_add_sea_model_Quad
#Multiplicative Seasonality
Mul_sea_model <- lm(log_t~Q1+Q2+Q3+Q4,data=cocacola_new)
summary(Mul_sea_model)
pred_Mul_sea_model <- data.frame(predict(Mul_sea_model,interval = 'predict',newdata = test))
rmse_Mul_sea_model <- sqrt(mean((cocacola_new$Sales-pred_Mul_sea_model$fit)^2,na.rm = T))
rmse_Mul_sea_model
#Multiplicative additive seasonality
Mul_sea_add_model <- lm(log_t~t+Q1+Q2+Q3+Q4,data=cocacola_new)
summary(Mul_sea_add_model)
pred_Mul_sea_add_model <- data.frame(predict(Mul_sea_add_model,interval = 'predict',newdata = test))
rmse_Mul_sea_add_model <- sqrt(mean((cocacola_new$Sales-pred_Mul_sea_add_model$fit)^2,na.rm = T))
rmse_Mul_sea_add_model

rmse_data <- data.frame(type=c('linear_model','exp_model','Quad_model','Additional','Additional_Quad','Multiplicative','multiplicative_add') ,rmse=c(rmse_pred_lm,rmse_pred_em,rmse_Quad_model,rmse_add_sea_model,rmse_add_sea_model_Quad,rmse_Mul_sea_model,rmse_Mul_sea_add_model))
View(rmse_data)
getwd()
write.csv(rmse_data,file='rmse_1')

pred_add_sea_model <- data.frame(predict(add_sea_model,interval="predict",newdata = test))
new <- cbind(cocacola,pred_add_sea_model)
View(new)
write.csv(new,file='prediction')
