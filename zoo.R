zoo <- read.csv('E:/excelr data/assignments/KNN/Zoo.csv')
zoo_new <- zoo[,-1]
boxplot(zoo_new[,1:6],main='boxplot_1')
boxplot(zoo_new[,7:12],main='boxplot_2')
boxplot(zoo_new[,13:17],main='boxplot_3')
summary(zoo_new)
str(zoo_new)
norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

norm_zoo <- as.data.frame(lapply(zoo_new[,1:16],norm),,na.rm=T)

# creation of training and testing data 
sampl_size <- floor(0.8*nrow(norm_zoo))
set.seed(777)
pick <- sample(seq_len(nrow(norm_zoo)),size = sampl_size)
train_zoo <- norm_zoo[pick,]
test_zoo <- norm_zoo[-pick,]

train_zoo_labels <-zoo[pick,18]
test_zoo_labels<- zoo[-pick,18]

library('class')

train_acc=NULL
test_acc=NULL

for(i in seq(1,70,1))
{
  zoo_pred <- knn(train_zoo,train_zoo,train_zoo_labels,k=i)
  train_acc <- c(train_acc,mean(zoo_pred==train_zoo_labels))
  zoo_test_pred <- knn(train_zoo,test_zoo,train_zoo_labels,k=i)
  test_acc <- c(test_acc,mean(zoo_test_pred==test_zoo_labels))
}
par(mfrow=c(1,2))
plot(seq(1,70,1),train_acc,type ='l',main='training_accuracy',col='red')
plot(seq(1,70,1),test_acc,type='l',main='testing_accuracy',col='black')

overall_prediction <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,70,1)))

library(ggplot2)
ggplot(overall_prediction,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


getwd()
write.csv(overall_prediction,file='zoo')

