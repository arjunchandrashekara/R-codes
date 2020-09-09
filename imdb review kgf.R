library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(scales)
library(ggplot2)
library(syuzhet)
library(reshape2)
library(dplyr)
library(lubridate)

# Review of the kannada movie KGF
KGF <- NULL
url <- "https://www.imdb.com/title/tt7838252/reviews?ref_=tt_ov_rt"
for(i in 1:20){
  url1 <- read_html(as.character(paste(url,i,sep="=")))
  rev <- url1 %>% html_nodes(".show-more__control")%>% html_text()
  
  KGF <- c(KGF,rev)
  
}
getwd()
write.table(KGF,"KGF reviews.txt")

# importing the dataset
kgf <- readLines("C:/Users/Arjun/Desktop/KGF reviews.txt")
dim(kgf)
str(kgf)

# using tm library
kgf.corpus <- Corpus(VectorSource(kgf))
kgf.corpus <- tm_map(kgf.corpus,tolower)
inspect(kgf.corpus[1:10])
kgf.corpus <- tm_map(kgf.corpus,removePunctuation)
inspect(kgf.corpus[1:5])
kgf.corpus <- tm_map(kgf.corpus,removeNumbers)
inspect(kgf.corpus[1:5])
stopwords <- readLines("E:/excelr data/textmining/stop.txt")
kgf.corpus <- tm_map(kgf.corpus,removeWords,stopwords)
inspect(kgf.corpus[1:5])
kgf.corpus <- tm_map(kgf.corpus,stripWhitespace)
inspect(kgf.corpus[1:5])

#creation of documenttermmatrix
dtm.kgf <- DocumentTermMatrix(kgf.corpus)
dtm.kgf <- as.matrix(dtm.kgf)
dim(dtm.kgf)

row_totals <- apply(dtm.kgf,1,sum)
dtm.new <- dtm.kgf[row_totals>0,]
class(dtm.new)

#barplot
w <- colSums(dtm.new)
w <- subset(w,w>=50)
barplot(w,las=2,col=rainbow(50))

#wordcloud
wordcloud(word=names(w),freq = w,random.order = FALSE,max.words = 150)

# sentimental analysis
senti_score <- get_nrc_sentiment(kgf)
head(senti_score)
#barplots of setimental analysis
barplot(colSums(senti_score),las=2,ylab="count",color=rainbow(10),main="barplots of setimental analysis")
