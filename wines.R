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

x <- new_data[,14:21]
d <- dist(x,method = 'manhattan')

x_clust <- hclust(d,method = 'complete')
plot(x_clust)
groups <- cutree(x_clust,3)
membreship <- as.matrix(groups)
new <- cbind(membreship,wine)
write.csv(new,file='wines_pc8')
table(membreship,wine$Type)
x <- membreship==wine$Type
table(x)
prop.table(table(x))*100
