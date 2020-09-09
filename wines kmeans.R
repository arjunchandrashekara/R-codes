getwd()
wine <- read.csv('E:/excelr data/assignments/PCA/wine.csv')
mydata <- scale(wine[,-1])

pca_mydata <- princomp(mydata,cor=T,scores = T,covmat = NULL)
summary(pca_mydata)
loadings(pca_mydata)
pca_mydata$scores[,1:8]
plot(pca_mydata)
pca_data <- as.data.frame(pca_mydata$scores[,1:8])
new_data <- cbind(mydata,pca_data)

#clustering using k means
wss = (nrow(pca_data)-1)*sum(apply(pca_data, 2, var))		 
for (i in 2:20) wss[i] = sum(kmeans(pca_data, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
install.packages("kselection")
library(kselection)
k <- kmeans(pca_data,6)
install.packages("animation")
library(animation)
k <- kmeans.ani(pca_data,6)
k$centers
final <- data.frame(pca_data,k$cluster)
View(final)
write.csv(final,'PCA_PC3')
