library(kernlab)
library(caret)
forest_fire <- read.csv('E:/excelr data/assignments/support vector machine/forestfires (1).csv')
boxplot(forest_fire[,3:10],main='before minimizing the outlier')
summary(forest_fire[,3:10])

FFMC_IQR <- 92.90-90.2
FFMC_min <- 90.20-(1.5*FFMC_IQR)

DMC_IQR <- 142.4-68.6
DMC_max <- 142.4+(1.5*DMC_IQR)

DC_IQR <- 713.9-437.7
DC_min <- 437.7-(1.5*DC_IQR)

ISI_IQR <- 10.8-6.5
ISI_max <- 10.8+(1.5*ISI_IQR)

temp_IQR <- 22.8-15.5
temp_min <- 15.5-(1.5*temp_IQR)

RH_IQR <- 53-33
RH_max <- 53+(1.5*RH_IQR)

wind_IQR <- 4.9-2.7
wind_max <- 4.9+(1.5*wind_IQR)

x=list(FFMC_min,DMC_max,DC_min,ISI_max,temp_min,RH_max,wind_max)
x
#outlier treatment
outlier <- function(x) {
  x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- mean(x)
  x
}

forest_fire_2 <- data.frame(lapply(forest_fire[,3:10],outlier),na.rm=T)

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
y <- data.frame(lapply(forest_fire_2,norm))
new_data <- cbind(forest_fire[,c(1,11:31)],y)
new_data <- (new_data[,c(22:29,2:20,21)])
boxplot(new_data[,2:8],main="boxplot after normalizing and minimizing the dataset")
# data partition
local_training <- createDataPartition(new_data$size_category,p=0.85,list=F)
training <- new_data[local_training,]
testing <- new_data[-local_training,]
# SVM analysis using vanilladot
attach(training)
svm_1 <- ksvm(training$size_category~.,data=training,kernel='vanilladot')
pediction_1 <- predict(svm_1,testing)
a <- table(pediction_1,testing$size_category)
accuracy1 <- (sum(diag(a))/sum(a))*100
accuracy1

# SVM analysis using rbfdot
svm_2 <- ksvm(size_category~.,data=training,kernel='rbfdot')
pediction_2 <- predict(svm_2,testing)
b <- table(pediction_2,testing$size_category)
accuracy2 <- (sum(diag(b))/sum(b))*100
accuracy2

# SVM analysis using polydot
svm_3 <- ksvm(size_category~.,data=training,kernel='polydot')
pediction_3 <- predict(svm_3,testing)
c <- table(pediction_3,testing$size_category)
accuracy3 <- (sum(diag(c))/sum(c))*100
accuracy3
#SVM analysis using tanhdot
svm_4 <- ksvm(size_category~.,data=training,kernel='tanhdot')
pediction_4 <- predict(svm_4,testing)
c <- table(pediction_4,testing$size_category)
accuracy4 <- (sum(diag(c))/sum(c))*100
accuracy4

overal_prediction <- data.frame(type=c('vanilladot','rbfdot','polydot','tanhdot'),accuracy=c(accuracy1,accuracy2,accuracy3,accuracy4))
getwd()
setwd("E:/excelr data/assignments/Decision Tree")
write.csv(overal_prediction,'forest_fire_2')

