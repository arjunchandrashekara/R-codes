library(rvest)
library(XML)
library(magrittr)
imdb_review <- NULL
rev <- NULL
url1 <- "https://www.imdb.com/title/tt5935704/reviews?ref_=tt_ov_rt"
url <- read_html(as.character(paste(url1,1,sep="")))
rev <- url %>%
  html_nodes(".show-more__control") %>%
  html_text()
  
imdb_review <- c(imdb_review,rev)
  
setwd("C:/Users/Arjun/Desktop")
write.table(imdb_review,"imdb_review.txt")

z <- readLines("C:/Users/Arjun/Desktop/imdb_review.txt")
dim(z)
str(z)

z.corpus <- Corpus(VectorSource(z))
z.corpus <- tm_map(z.corpus,removePunctuation)
z.corpus <- tm_map(z.corpus,removeNumbers)
stopwords <- readLines("E:/excelr data/textmining/stop.txt")
z.corpus <- tm_map(z.corpus,removeWords,stop_words)
z.corpus <- tm_map(z.corpus,stripWhitespace)

tdm.z <- TermDocumentMatrix(z.corpus)
as.matrix(tdm.z)

dtm.z <- t(tdm.z)
dtm.z$nrow
dtm.z$ncol

rowtotals <- apply(dtm.z,1,sum)
dtm.new <- dtm.z[rowtotals>0,]

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