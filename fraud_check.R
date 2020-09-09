#install.packages("randonForest")
library(randomForest)

fraud_check_1 <- read.csv("E:/excelr data/assignments/random forest/Fraud_check.csv")

# boxplots showing there is no outliers
boxplot(fraud_check_1$City.Population,main='boxplot of city.population')
boxplot(fraud_check_1$Work.Experience,main ="boxplot of work.experience")
#histogram
hist(fraud_check_1$City.Population,main='histogram of city.population')
hist(fraud_check_1$Work.Experience,main='histogram of work.experience')
#scaling the data
a <- data.frame(scale(fraud_check_1[,4]))
b <- data.frame(scale(fraud_check_1[,5]))
fraudcheck <- cbind(fraud_check_1[,-c(4,5)],a,b)
colnames(fraudcheck)[c(5,6)] <- c('city.population','work_experience')
#histogram after scaling
hist(fraudcheck$city.population,main='histogram of city.population')
hist(fraudcheck$work_experience,main='histogram of work.experience')

X <- as.data.frame(fraudcheck[,3])
type <- function(x){
  return(ifelse(x <= 30000,"risky","good"))
}
tax<- data.frame(lapply(X,type),na.rm=T)
View(tax)
colnames(tax) <- "Taxable.income"
new_data <- cbind(fraudcheck[,-3],tax)

# selecting random test and train data
sample_size <- floor(0.8*nrow(new_data))
pick <- sample(seq_len(nrow(new_data)),sample_size)
test <- new_data[-pick,]
train <- new_data[pick,]
attach(train)
# building the model with random forest
fit.forest <- randomForest(train$Taxable.income~.,data=train,importance= T)

mean(train$Taxable.income==predict(fit.forest,train))
mean(test$Taxable.income==predict(fit.forest,test))

prediction <- predict(fit.forest,test)
prediction
library(caret)
confusionMatrix(test$Taxable.income,prediction)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

library(gmodels)
rf_perf<-CrossTable(test$Taxable.income, prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

