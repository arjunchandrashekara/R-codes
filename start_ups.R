start_ups <- read.csv('E:/excelr data/assignments/neural networks/50_Startups.csv')
start_ups_new <- start_ups[,-4]
start_ups_scaled <- as.data.frame(scale(start_ups_new))
train_data <- start_ups_scaled[1:40,]
test_data <- start_ups_scaled[41:50,]

library(neuralnet)
#without hidden layer
model1 <- neuralnet(Profit~.,data=train_data)
plot(model1)
results <- compute(model1,test_data[,1:3])
pred <- results$net.result
cor(pred,test_data$Profit)
plot(pred,test_data$Profit)

#with single hidden layer
model2 <- neuralnet(Profit~.,start_ups_scaled,hidden = 5)
plot(model2)
results_model2 <- compute(model2,test_data[,1:3])
pred_model2 <- results_model2$net.result
cor(pred_model2[1:10,],test_data$Profit)
plot(pred_model2[1:10,],test_data$Profit)

#with multiple hidden layer
model3 <- neuralnet(Profit~.,start_ups_scaled,hidden = c(5,2))
plot(model3)
results_model3 <- compute(model3,test_data[,1:3])
pred_model3 <- results_model3$net.result
cor(pred_model3,test_data$Profit)

model4 <- neuralnet(Profit~.,start_ups_scaled,hidden = c(5,3))
plot(model4)
results_model4 <- compute(model4,test_data[,1:3])
pred_model4 <- results_model4$net.result
cor(pred_model4,test_data$Profit)
