bank_full <- read.csv('E:/excelr data/assignments/logistic regression/bank-full.csv')
View(bank_full)
str(bank_full)
logit <- glm(y~age+factor(default)+balance+factor(housing)+factor(loan)+day+duration+campaign,data=bank_full,family='binomial')
summary(logit)
logit1 <- glm(y~factor(default)+balance+factor(housing)+factor(loan)+day+duration+campaign,data=bank_full,family='binomial')
summary(logit1)
exp(coef(logit1))
table(bank_full$y)
pred <- as.data.frame(predict(logit1,type=c('response'),bank_full))
View(pred)
confusion <- table(pred>0.5,bank_full$y)
confusion
Accuracy <- (sum(diag(confusion))/sum(confusion))*100
Accuracy
error <- 100-Accuracy
error
invoke <- function(x){return(ifelse(x<0.5,"No","Yes"))}
pred <- as.data.frame(apply(pred,MARGIN = 2,invoke))
final <- cbind(bank_full,pred)
write.csv(final,"bank_full_credit")