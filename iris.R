library(randomForest)
data(iris)
View(iris)
boxplot(iris[,c(1,2,3,4)])
#outlier treatment
out <- function(x) {
  x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- mean(x)
  x
}
a <- data.frame(lapply(iris[,-5],out))
boxplot(a)
iris_new <- cbind(a,iris[,5])
colnames(iris_new)[5] <- "Species"
attach(iris_new)
table(Species)
library(caret)
sample_size <- floor(0.8*nrow(iris_new))
pick <- sample(seq_len(nrow(iris_new)),sample_size)
train <- iris_new[pick,]
test <- iris_new[-pick,]
fit_forest <- randomForest(train$Species~.,data=train,na.action=na.roughfix,importance=TRUE)
fit_forest$ntree
mean(train$Species==predict(fit_forest,train))
pred_test <- predict(fit_forest,newdata=test)
pred_test
mean(pred_test==test$Species)
library(gmodels)
rf_perf<-CrossTable(test$Species, pred_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
