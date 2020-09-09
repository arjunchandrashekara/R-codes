glass_1<- read.csv('E:/excelr data/assignments/KNN/glass.csv')

boxplot(glass_1[,1:5],main='boxplot_1')
boxplot(glass_1[,6:9],main='boxplot_2')

summary(glass_1)

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_norm <- as.data.frame(lapply(glass[,-10],norm))

train_glass <- glass_norm[1:160,]
test_glass <- glass_norm[161:214,]

train_glass_labels <- glass[1:160,10]
test_glass_labels <- glass[161:214,10]

train_acc <- NULL
test_acc <- NULL

library('class')
for(i in seq(3,150,2))
{
  glass_train_pred <- knn(train_glass,train_glass,train_glass_labels,k=i)
  train_acc <- c(train_acc,mean(glass_train_pred==train_glass_labels))
  glass_test_pred <-knn(train_glass,test_glass,train_glass_labels,k=i)
  test_acc <- c(test_acc,mean(glass_test_pred==test_glass_labels))
}
par(mfrow=c(1,2))
plot(seq(3,150,2),train_acc,type='l',main="training_accuracy",col='blue')
plot(seq(3,150,2),test_acc,type = 'l',main="testing_accuracy",col='red')

overall_prediction <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,150,2)))

library(ggplot2)
ggplot(overall_prediction,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


getwd()
setwd("E:/excelr data/assignments/KNN")
write.csv(overall_prediction,file='glass')
