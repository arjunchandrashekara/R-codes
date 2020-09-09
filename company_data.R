library(randomForest)
company_data_1 <- read.csv("E:/excelr data/assignments/random forest/Company_Data.csv")
boxplot(company_data_1[,c(2:6,8,9)])
#outlier treatment
outlier <- function(x) {
  x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- mean(x)
  x
}
p <- data.frame(apply(company_data_1[,c(2:6,8,9)],2,outlier))
company_data <- cbind(company_data_1[,-c(2:6,8,9)],p)
X <- function(x){
  return(ifelse(x<8,"low","high"))
}
a <- as.data.frame(company_data[,1])
seg_1 <- data.frame(lapply(a,X))
company <- cbind(seg_1,company_data[,-1])
colnames(company)[1] <- "Sales"

norm <- function(x){
  return((x-min(x)/(max(x)-min(x))))
}
b <- data.frame(lapply(company[,c(5:11)],norm))
company_new <- cbind(company[,-c(5:11)],b)

#spliting the data into train and test
sample_size <- floor(0.8*nrow(company_new))
pick <- sample(seq_len(nrow(company_new)),sample_size)
train <- company_new[pick,]
test <- company_new[-pick,]

#model building stage
fit.forest <- randomForest(train$Sales~.,data=train,na.action = na.roughfix,importance=T)

mean(train$Sales==predict(fit.forest,train))*100
mean(test$Sales==predict(fit.forest,test))*100

prediction <- predict(fit.forest,test)
library(caret)
confusionMatrix(test$Sales,prediction)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

summary(fit.forest)

library(gmodels)
rf_perf<-CrossTable(test$Sales, prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

