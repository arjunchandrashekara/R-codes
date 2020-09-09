concrete <- read.csv('E:/excelr data/assignments/neural networks/concrete.csv')
norm_concrete <- function(x){return((x-min(x))/(max(x)-min(x)))}

concrete_norm <- as.data.frame(lapply(concrete,norm_concrete))
train_data <- concrete_norm[1:700,]
test_data <- concrete_norm[701:1030,]

library(neuralnet)

#without any hidden layers
model1 <- neuralnet(strength~.,train_data)
plot(model1)
results_model1 <- compute(model1,test_data[,1:8])
pred_model1 <- results_model1$net.result
cor(pred_model1,test_data$strength)

#with single hidden layer
model2 <- neuralnet(strength~.,train_data,hidden = 5)
plot(model2)
results_model2 <- compute(model2,test_data[,1:8])
pred_model2 <- results_model2$net.result
cor(pred_model2,test_data$strength)

#with multi hidden layer

model3 <- neuralnet(strength~.,train_data,hidden = c(5,2))
plot(model3)
results_model3 <- compute(model3,test_data[,1:8])
pred_model3 <- results_model3$net.result
cor(pred_model3,test_data$strength)