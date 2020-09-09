forestfires <- read.csv('E:/excelr data/assignments/neural networks/forestfires (1).csv')
forestfires <- forestfires[,-c(1,2,31)]
norm_data <- function(x){return((x-min(x))/(max(x)-min(x)))}

norm_ff <- as.data.frame(lapply(forestfires,norm_data))
train_data <- norm_ff[1:400,]
test_data <- norm_ff[401:517,]

library(neuralnet)

model1 <- neuralnet(area~.,data=norm_ff)
plot(model1)
results_model1 <- compute(model1,test_data[,-9])
pred_model1 <- results_model1$net.result
cor(pred_model1,test_data$area)

model2 <- neuralnet(area~.,data=norm_ff,hidden=5)
plot(model2)
results_model2 <- compute(model2,test_data[,-9])
pred_model2 <- results_model2$net.result
cor(pred_model2,test_data$area)

model3 <- neuralnet(area~.,data=norm_ff,hidden = c(5,2))
plot(model3)
results_model3 <- compute(model3,test_data[,-9])
pred_model3 <- results_model3$net.result
cor(pred_model3,test_data$area)