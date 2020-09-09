library('kernlab')
train_1 <- read.csv("E:/excelr data/assignments/support vector machine/SalaryData_Train(1).csv")
test_1 <- read.csv("E:/excelr data/assignments/support vector machine/SalaryData_Test(1).csv")

boxplot(train_1[,c(1,4,10,12)],main='train')
boxplot(test_1[,c(1,4,10,12)],main='test')
boxplot(train_1[,c(1,4,12)],main='train')
summary(train_1[,c(1,4,10,12)])
summary(test_1[,c(1,4,10,12)])
boxplot(test_1[,c(1,4,10,12)],main='test')
# findings of the boxplot in train data
age_IQR <- 47-28
age_max <- 47+(1.5*age_IQR)

educationno_IQR <- 13-9
educationno_min <- 13-(1.5*educationno_IQR)

hoursperweek_IQR <- 45-40
hoursperweek_max <- 45+(1.5*hoursperweek_IQR)
hoursperweek_min <- 40-(1.5*hoursperweek_IQR)

# findings of the boxplot in test data
age_IQR_t <- 48-28
age_max_t <- 47+(1.5*age_IQR_t)

educationno_IQR_t <- 13-9
educationno_min_t <- 13-(1.5*educationno_IQR_t)

hoursperweek_IQR_t <- 45-40
hoursperweek_max_t <- 45+(1.5*hoursperweek_IQR)
hoursperweek_min_t <- 40-(1.5*hoursperweek_IQR)

#outlier treatment in test and train data
outlier <- function(x) {
  x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- mean(x)
  x
}

train_new <- data.frame(train_1[,c(1,4,10,12)])
test_new <- data.frame(test_1[,c(1,4,10,12)])

train_a <- data.frame(lapply(train_new,outlier),na.rm=T)
test_b <- data.frame(lapply(test_new,outlier),na.rm=T)

norm <- function(x){
  returm((x-min(x))/(max(x)-min(x)))
}
train_new_2 <- data.frame(lapply(train_a,norm),na.rm=T)
test_new_2 <- data.frame(lapply(train_b,norm),na.rm=T)

train <- cbind(train_a,train_1[,-c(1,4,10,12)])
test <- cbind(test_b,test_1[,-c(1,4,10,12)])


# using vanilldot kernal
attach(train)
classification_1 <- ksvm(Salary~.,data=train,kernel="vanilladot")
prediction_1 <- predict(classification_1,newdata=test)

table(prediction_1,test$Salary)
x <- prediction_1==test$Salary
table(x)
a <- prop.table(table(x))
a

# using rbfdot kernal
classification_2 <- ksvm(Salary~.,data=train,kernel="rbfdot")
prediction_2 <- predict(classification_2,newdata=test)

table(prediction_2,test$Salary)
y <- prediction_2==test$Salary
table(y)
b <- prop.table(table(y))
b


#polydot
classification_3 <- ksvm(Salary~.,data=train,kernel="polydot")
prediction_3 <- predict(classification_3,newdata=test)
           
table(prediction_3,test$Salary)
z <- prediction_3==test$Salary
table(z)
c <- prop.table(table(z))
c

#tanhdot           
classification_4 <- ksvm(Salary~.,data=train,kernel="tanhdot")
prediction_4 <- predict(classification_4,newdata=test)

table(prediction_4,test$Salary)
p <- prediction_4==test$Salary
table(z)
d <- prop.table(table(z))
d
overall_prediction <- data.frame(type=c('vanilladot','rbfdot','polydot','tanhdot'),accuracy=c(a,b,c,d))

write.csv(overall_prediction,'salary_data')
