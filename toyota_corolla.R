library(readxl)
toyota_corolla <- read_excel('E:/excelr data/assignments/multi linear regression/toyotaCorolla.xlsx')
View(toyota_corolla)
toyota_corolla_norm <- as.data.frame(scale(toyota_corolla))
pairs(toyota_corolla_norm)
cor(toyota_corolla_norm)
attach(toyota_corolla_norm)
#Model 1 without transformation
toyota_corolla_model <- lm(Price~.,data=toyota_corolla_norm)
summary(toyota_corolla_model)
# Model 2 upon applying the log transformation on dependent Variable
toyota_corolla_model_2 <- lm(log(Price)~.,data=toyota_corolla_norm)
summary(toyota_corolla_model_2)
# Model 3 upon keeping the variable as it is and removing the influenced paramtres
library(car)
vif(toyota_corolla_model)
avPlots(toyota_corolla_model)
influencePlot(toyota_corolla_model)
toyota_corolla_model_3 <- lm(Price~.-Doors,data=toyota_corolla_norm[-c(81,222,961),])
summary(toyota_corolla_model_3)
confint(toyota_corolla_model_3,level=0.95)
prediction <- as.data.frame(predict(toyota_corolla_model_3,interval="prediction"))
new <- cbind(toyota_corolla_norm[-c(81,222,961),],prediction)
write.csv(new,file='toyota_corolla_fit')

