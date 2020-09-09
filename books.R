install.packages('recommender lab',dependencies = TRUE)
install.packages('Matrix')
library("recommenderlab")
library(caTools)

book <- read.csv('E:/excelr data/assignments/Recommendation/books.csv')

str(book)

colnames(book)[6] <- "ratings"
book1 <- book[,c(5,6)]
hist(book$ratings)

book_matrix <- as(book1,"realRatingMatrix")

?realRatingMatrix

book_matrix_reco <- Recommender(book_matrix,"UBCF")

recommended_items1 <- predict(book_matrix_reco, book_matrix[1:10,], n=10)
as(recommended_items1, "list")
