#install.packages("twitteR")
#install.packages("rtweet")
#install.packages("ROAuth")
library("ROAuth")
library("twitteR")

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('BarackObama', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
setwd("C:/Users/Arjun/Desktop")
write.table(TweetsDF, "Tweets.txt",row.names = F)
write.csv(TweetsDF, "Tweets.csv",row.names = F)

library(tm)
library(topicmodels)
library(slam)

x <- read.csv('C:/Users/Arjun/Desktop/Barack Obama.csv') #importing Barack Obama file
x1 <- x$text
class(x1)
str(x)
length(x)
str(x1)
length(x1)

#using tm Package
corpus.x <- Corpus(VectorSource(x1))
corpus.x <- tm_map(corpus.x,removePunctuation)
inspect(corpus.x[1:10])
corpus.x <- tm_map(corpus.x,removeNumbers)
inspect(corpus.x[1:10])
stop_words <- readLines("E:/excelr data/textmining/stop.txt")
corpus.x <- tm_map(corpus.x,removeWords,stop_words)
inspect(corpus.x[1:10])
corpus.x <- tm_map(corpus.x,stripWhitespace)
inspect(corpus.x[1:10])
removeurl <- function(x) gsub("http[[:alnum:]]*","",x)
corpus.x <- tm_map(corpus.x,content_transformer(removeurl))
inspect(corpus.x[1:10])

#build a termdocument Matrix
tdm.x1 <- TermDocumentMatrix(corpus.x)
as.matrix(tdm.x1)
dim(tdm.x1)

dtm.x1 <- t(tdm.x1)
dim(dtm.x1)
dtm.x1$nrow
dtm.x1$ncol
rowtotals <- apply(dtm.x1,1,sum)
rowtotals
dtm.new <- dtm.x1[rowtotals>0,]
dtm.new

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

dtm.new <- as.matrix(dtm.new)
# Barplot 
y <- colSums(dtm.new)
y <- subset(y,y>=25)
barplot(y,las=2,col=c('blue','red'),main="barplot for term freequency")
#wordcloud
library(wordcloud)

wordcloud(words=names(y),freq=y,max.words = 150,random.order = FALSE)

#sentimental analysis
library(syuzhet)
library(lubridate)
library(dplyr)
library(scales)
library(reshape2)

z <- as.character(x1)
View(z)

senti_score <- get_nrc_sentiment(z)
head(senti_score)
barplot(colSums(senti_score),las=2,ylab="count",main = "barplot for Sentimental score",col = c('orange','skyblue','green'))
