library(readr)
crime_data <- read.csv('E:/excelr data/assignments/clustering/crime_data.csv')
View(crime_data)
crime_data_norm <- scale(crime_data[,-1])
d <- dist(crime_data_norm,method = 'euclidean')
fit <- hclust(d,method="single")
plot(fit)
plot(fit,hang=-1)
membreship <- as.data.frame(as.matrix(cutree(fit,k=5)))
cluster <- rect.hclust(fit,k=5,border = 'red')
fit_final<- cbind(crime_data,membreship)
library(dplyr)
fit_final_new <-  rename(fit_final,membreship=V1)
fit_final_new <- fit_final_new[,c(1,6,2:5)]
write.csv(fit_final_new,file="crime_data_clustered_new")


