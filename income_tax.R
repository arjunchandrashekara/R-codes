install.packages('tree')
install.packages('party')
library(readr)
library(C50)
library(caret)
library(tree)
library(party)
library(gmodels)

fraud_check <- read.csv('E:/excelr data/assignments/Decision Tree/Fraud_check.csv')

hist(fraud_check$Taxable.Income)

X <- data.frame(fraud_check[,3])

cat_data <- function(x){
  return(ifelse(x<=30000,"risky","good"))
}

cat_data <- data.frame(lapply(X,cat_data))
new_data <- cbind(cat_data,fraud_check[,-3])
colnames(new_data)[1] <- "tax"

new <- data.frame(scale(new_data[,c(4,5)]))
new_data <- cbind(new_data[,-c(4,5)],new)

local_training <- createDataPartition(new_data$tax,p=0.75,list = F)
training <- new_data[local_training,]
testing <- new_data[-local_training,]

model <- ctree(training$tax~.,data=training)
pred <- predict(model,testing[,-1])
b <- table(pred,testing$tax)
b
summary(model)
plot(model)
CrossTable(testing$tax,pred)
# for loop
accuracy=NULL
for(i in 1:50)
{
  local_training <- createDataPartition(new_data$tax,p=0.75,list = F)
  training <- new_data[local_training,]
  testing <- new_data[-local_training,]
  
  model_1 <- C5.0(training$tax~.,data=training)
  pred_1 <- predict(model_1,testing[,-1])
  b <- table(pred_1,testing$tax)
  
  accuracy <- c(accuracy,c((sum(diag(b))/sum(b))*100))
}  

summary(accuracy)
p <- data.frame(list(accuracy=accuracy,i=1:50))
p
setwd("E:/excelr data/assignments/Decision Tree")
write.csv(p,file='fraud_data')


