install.packages("wordcloud")
install.packages("MLmetrics")

library(caret)
library(e1071)
library(wordcloud)
library(MLmetrics)
library(tm)
#importing the dataset
text <- read.csv("E:/excelr data/assignments/Naive Bayes/sms_raw_NB.csv")
#finding the content of the dataset
str(text)
table(text$type)
prop.table(table(text$type))*100
# creation of train and text data
sample_size <- floor(0.8*nrow(text))
pick <- sample(seq_len(nrow(text)),sample_size)
train <- text[pick,]
test <- text[-pick,]

#using tm package#
text.corpus <- Corpus(VectorSource(train$text))
text.corpus
text.corpus <- tm_map(text.corpus,removePunctuation)
text.corpus
my_stopwords <- readLines('E://excelr data//textmining//stop.txt')
text.corpus <- tm_map(text.corpus,removeWords,my_stopwords)
text.corpus <- tm_map(text.corpus,removeNumbers)
text.corpus <- tm_map(text.corpus,stripWhitespace)
#data visulization
par(mfrow = c(1,2))
wordcloud(text.corpus[train$type == "ham"], min.freq = 40, random.order = FALSE, colors = "green",main="train_ham")
wordcloud(text.corpus[train$type == "spam"], min.freq = 40, random.order = FALSE, colors = "red",main="train_spam")
#creation of dtm
dtm <- DocumentTermMatrix(text.corpus)
inspect(dtm[1:5,5:10])

text_features <- findFreqTerms(dtm,5)
summary(text_features)
tail(text_features)

dtm_train <- DocumentTermMatrix(text.corpus,list(global = c(2, Inf), dictionary = text_features))

convert_counts <- function(x){
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
  return (x)
}
dtm_train <- apply(dtm_train, MARGIN = 2, convert_counts)


classification <- naiveBayes(dtm_train,train$type)

#using tm package#
test.text.corpus <- Corpus(VectorSource(test$text))
test.text.corpus
test.text.corpus <- tm_map(test.text.corpus,removePunctuation)
test.text.corpus
my_stopwords <- readLines('E://excelr data//textmining//stop.txt')
test.text.corpus <- tm_map(test.text.corpus,removeWords,my_stopwords)
test.text.corpus <- tm_map(test.text.corpus,removeNumbers)
test.text.corpus <- tm_map(test.text.corpus,stripWhitespace)

#data visulization
par(mfrow = c(1,2))
wordcloud(test.text.corpus[test$type == "ham"], min.freq = 40, random.order = FALSE, colors = "green",main='test_ham')
wordcloud(test.text.corpus[test$type == "spam"], min.freq = 40, random.order = FALSE, colors = "red",main='test_ham')

dtm_test <- DocumentTermMatrix(test.text.corpus, list(global = c(2, Inf), dictionary = text_features))
dtm_test <- apply(dtm_test, MARGIN = 2, convert_counts)


test_pred <- predict(classification,dtm_test)
table(test$type,test_pred)
confusionMatrix(test$type,test_pred)
