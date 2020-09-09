library(kernlab)
forest_fire <- read.csv('E:/excelr data/assignments/support vector machine/forestfires (1).csv')
x <- data.frame(scale(forest_fire[,3:9]))
new_data <- cbind(forest_fire[,c(1:2,10:31)],x)
# data partition
train <- new_data[1:362,]
test <- new_data[363:517,]


# SVM analysis using vanilladot
attach(train)
svm_1 <- ksvm(size_category~.,data=train,kernel='vanilladot')
pediction_1 <- predict(svm_1,test)
a <- table(pediction_1,test$size_category)
accuracy1 <- (sum(diag(a))/sum(a))*100
accuracy1

# SVM analysis using rbfdot
svm_2 <- ksvm(size_category~.,data=train,kernel='rbfdot')
pediction_2 <- predict(svm_2,test)
b <- table(pediction_2,test$size_category)
accuracy2 <- (sum(diag(b))/sum(b))*100
accuracy2

# SVM analysis using polydot
svm_3 <- ksvm(size_category~.,data=train,kernel='polydot')
pediction_3 <- predict(svm_3,test)
c <- table(pediction_3,test$size_category)
accuracy3 <- (sum(diag(c))/sum(c))*100
accuracy3