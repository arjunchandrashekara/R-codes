library(WriteXLS)
creditcard <- read.csv('E:/excelr data/assignments/logistic regression/creditcard.csv')
View(creditcard)
creditcard <- creditcard[,-1]
View(creditcard)
model <- glm(card~reports+age+income+share+expenditure+factor(owner)+factor(selfemp)+dependents+months+factor(majorcards)+active,data=creditcard,family = 'binomial')
summary(model)
exp(coef(model))
pred <- as.data.frame(predict(model,type=c('response'),creditcard))
View(pred)
confusion <- table(pred>0.5,creditcard$card)
confusion
Accuracy <- sum(diag(confusion))/sum(confusion)
Accuracy
Invoke <- function(x){return(ifelse(x<0.5,'No','Yes'))
}
pred_dataframe <- as.data.frame(apply(pred,MARGIN = 2,Invoke))
View(pred_dataframe)
final <- cbind(creditcard,pred_dataframe)
final <- final[,c(1,13,2:12)]
getwd()
write.csv(final,"credit_fitted")
