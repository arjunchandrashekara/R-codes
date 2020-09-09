#install.packages("rvest")
#install.packages("XML")
library(rvest)
library(XML)
library(magrittr)
# Extracting reviews of iphone 6 in amazon
aurl <- "https://www.amazon.in/Apple-iPhone-6S-Gold-128GB/dp/B016QBU4S4/ref=sr_1_2?dchild=1&keywords=iphone+6&qid=1586455817&sr=8-2#customerReviews"
amazon_review <- NULL
for(i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_review <- c(amazon_review,rev)

}
getwd()
setwd('C:/Users/Arjun/Desktop')
write.table(amazon_review,"iphone_6.txt")
write.csv(amazon_review,'iphone_6.csv')


iphone <- readLines("C:/Users/Arjun/Desktop/iphone_6.txt")
dim(iphone)
str(iphone)

# using tm package
library(tm)
iphone.corpus <- Corpus(VectorSource(iphone))
iphone.corpus <- tm_map(iphone.corpus,removePunctuation)
iphone.corpus <- tm_map(iphone.corpus,tolower)
stopwords <- readLines("E:/excelr data/textmining/stop.txt")
iphone.corpus <- tm_map(iphone.corpus,removeWords,stopwords)
iphone.corpus <- tm_map(iphone.corpus,removeNumbers)
iphone.corpus <- tm_map(iphone.corpus,stripWhitespace)
removeUrl <- function(x)gsub("http[[:alnum:]]*",'',x)
iphone.corpus <- tm_map(iphone.corpus,content_transformer(removeUrl))
iphone.corpus <- tm_map(iphone.corpus,gsub,patter="apps",replacement="app")
inspect(iphone.corpus[1:5])

#creation of dtm matrix
tdm.iphone <- TermDocumentMatrix(iphone.corpus)
tdm.iphone <- as.matrix(tdm.iphone)
dim(tdm.iphone)

dtm.iphone <- t(tdm.iphone)
class(dtm.iphone)
dim(dtm.iphone)

rowtotals <- apply(dtm.iphone,1,sum)

dtm.new <- dtm.iphone[rowtotals>0,]

lda <- LDA(dtm.new,10)

lterm <- terms(lda,1)
lterm

tops <- terms(lda)
tops
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)

tb
cls <- hclust(dist(tb),method = 'ward.D2')
par(family ='HiraKakuProN-W3')
plot(cls)

a <- colSums(dtm.iphone)
dtm.iphone[1:20,1:10]
#barplots
a1 <- subset(a,a>=5)
barplot(a1,las=2,color="red")
# creation of the wordcloud
library(wordcloud)
#install.packages('RColorBrewer')
w <- sort(a1,decreasing = TRUE)
wordcloud(word=names(w),freq = w,max.words = 150)

#to obtain the sentimental analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

senti_scores <- get_nrc_sentiment(iphone)
class(senti_scores)
senti_scores[10:45,]

#barplot
barplot(colSums(senti_scores),las=2,color=rainbow(20),ylab="count",main = "sentiment score of apple iphone_6")

