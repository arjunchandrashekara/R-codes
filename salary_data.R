install.packages('kernlab')
library('kernlab')
test <- read.csv("E:/excelr data/assignments/support vector machine/SalaryData_Test(1).csv")
train <- read.csv("E:/excelr data/assignments/support vector machine/SalaryData_Train(1).csv")

# using vanilldot kernal
attach(train)

classification_1 <- ksvm(Salary~.,data=train,kernel="vanilladot")
prediction_1 <- predict(classification_1,newdata=test)

table(prediction_1,test$Salary)
x <- prediction_1==test$Salary
table(x)
prop.table(table(x))

# using rbfdot kernal
classification_2 <- ksvm(Salary~.,data=train,kernel="rbfdot")
prediction_2 <- predict(classification_2,newdata=test)

table(prediction_2,test$Salary)
y <- prediction_2==test$Salary
table(y)
prop.table(table(y))

#polydot
classification_3 <- ksvm(Salary~.,data=train,kernel="polydot")
prediction_3 <- predict(classification_3,newdata=test)

table(prediction_3,test$Salary)
z <- prediction_3==test$Salary
table(z)
prop.table(table(z))
