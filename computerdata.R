library(readr)
computer_data <- read.csv('E:/excelr data/assignments/multi linear regression/Computer_Data.csv')
View(computer_data)
computer_data <- computer_data[,-1]
View(computer_data)
normal_data <- function(x){
  return(ifelse(x=='yes',1,0))
}
computer_data_norm <- as.data.frame(apply(computer_data[,c(6,7,8)],MARGIN = 2,normal_data))
View(computer_data_norm)
computer_data_new <- cbind.data.frame(computer_data[,-c(6,7,8)],computer_data_norm)
View(computer_data_new)
summary(computer_data_new)
normaliztion_data <- function(x){
  return(x <- (x-min(x))/(max(x)-min(x)))
}
computer_data_new <- as.data.frame(apply(computer_data_new,MARGIN=2,normaliztion_data))
View(computer_data_new)
summary(computer_data_new)
pairs(computer_data_new)
cor(computer_data_new)
#Model 1 without any transformation
computer_data_new_model <- lm(price~.,data=computer_data_new)
summary(computer_data_new_model)
#Model 2 on removing influencing parametre
library(car)
vif(computer_data_new_model)
avPlots(computer_data_new_model)
influencePlot(computer_data_new_model)
computer_data_new_model_2 <- lm(price~.-multi,data=computer_data_new)
summary(computer_data_new_model_2)
#model 3 on removing influencing parametre and rows and transformation
computer_data_new_model_3 <- lm(price ~.-multi,data=computer_data_new[-c(1441,1701,3784,4478),])
summary(computer_data_new_model_3)
confint(computer_data_new_model,level=0.95)
prediction <- as.data.frame(predict(computer_data_new_model,interval='predict'))
new <- cbind(computer_data_new,prediction)
write.csv(new,file='computer_data')
