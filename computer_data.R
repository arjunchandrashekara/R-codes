library(readr)
library(C50)
library(caret)

company_data <- read.csv('E:/excelr data/assignments/Decision Tree/Company_Data.csv')
x <- data.frame(company_data[,1])
catagorise <- function(x){
  return(ifelse(x<8.0,"low","high"))}

cat_data <- data.frame(apply(x,MARGIN=2,catagorise))
new_data <- cbind(cat_data,company_data[,-1])

colnames(new_data)[1] <- "Sales"

#Data Partition
localtraining <- createDataPartition(new_data$Sales,p=0.8,list=F)
training <- new_data[localtraining,]
testing <- new_data[-localtraining,]
table(new_data$Sales)

# Creating the model
model <- C5.0(training$Sales~.,data=training,trails=40)
summary(model)
plot(model)
pred <- predict(model,testing[,-1])
table(pred)
a <- table(testing$Sales,pred)
a
accuracy <- (sum(diag(a)/sum(a)))*100
accuracy


#run a for loop 
acc=NULL
for(i in 1:100)
{
  print(i)
  localtraining <- createDataPartition(new_data$Sales,p=0.8,list=F)
  training1 <- new_data[localtraining,]
  testing1 <- new_data[-localtraining,]
  
  model1 <- C5.0(training1$Sales~.,data=training1)
  summary(model1)
  plot(model1)
  pred1 <- predict(model1,testing1[,-1])
  a <- table(testing1$Sales,pred1)
  acc <- c(acc,(sum(diag(a)/sum(a)))*100)
}

summary(acc)
y <- data.frame(list(acc=acc,i=1:100))
View(y)
write.csv(y,file="computer_data")
z <- mean(y$acc)
z
setwd()
getwd()
