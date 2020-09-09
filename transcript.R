Sys.getenv("R_ARCH")
Sys.setenv(JAVA_HOME= 'C:/Program Files/Java/jre1.8.0_261')
rm(list=ls())
options(java.parameters = "-Xmx1024m")
library(wordcloud2)
library(rJava)
library("rvest")
library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(NLP)
library(qdap)
library(ggplot2)
library(textclean)
library(tidytext)
library(tibble)
#reading the text file
text_data <- readLines("E:/topic modelling/final.txt")
data = data.frame(id = 1:length(text_data), text = text_data, stringsAsFactors = F)
dim(data)
stopwords <- readLines("E:/excelr data/textmining/stop.txt")
names <- c("Mounica","Patel","Ananya","Visitor")
#cleaning the data
x  =  removeWords(data$text,stopwords )           # removing stopwords created above
x  =  stripWhitespace(x)
x = removeNumbers(x)
x=removePunctuation(x)
x=removeWords(x,names )
x.corpus <- Corpus(VectorSource(x))
setwd("C:/Users/Arjun/Desktop")
#Unigram
data_uni <- data.frame(table(NGramTokenizer(x.corpus, Weka_control(min = 1, max =1 ))))
wordcloud2(data_uni)
write.csv(data_uni,"11111")
#bigram
data_bi<- data.frame(table(NGramTokenizer(x.corpus, Weka_control(min = 2, max =2 ))))
write.csv(data_bi,"bigram.csv")
wordcloud2(data_bi)
#trigram
data_tri <-data.frame(table(NGramTokenizer(x.corpus, Weka_control(min = 3, max =3 ))))
wordcloud2(data_tri) 
write.csv(data_tri,"trigram.csv")
getwd()

#BigramTokenizer <- function(x) {
 # NGramTokenizer(x, Weka_control(min = 2, max = 2))
#} 
#dtm.bigram = DocumentTermMatrix(x.corpus,
  #                              control = list(tokenize = BigramTokenizer))



#dtm.bigram <- removeSparseTerms(dtm.bigram,0.99)
#dtm.bigram <- as.matrix(dtm.bigram)
#row_totals <- apply(dtm.bigram,1,sum)
#dtm.bigram <- dtm.bigram[row_totals>0,]
#class(dtm.bigram)
#dim(dtm.bigram)
#freq2 <- colSums(dtm.bigram)
#windows()
#pal <- brewer.pal(8,"Dark2")
#wordcloud(names(freq2), freq2, min.freq=6000,colors = pal)
BigramTokenizer <- function(x) NGramTokenizer(x.corpus, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(x.corpus,
                                control = list(tokenize = BigramTokenizer))
tdm.bigram <- removeSparseTerms(tdm.bigram,0.99)
col_totals <- apply(tdm.bigram,2,sum)
tdm.bigram <- tdm.bigram[,col_totals>0]
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
#barplot
w <- subset(freq,freq>5000)
w <- as.data.frame(w)
write.csv(w,"w.csv")
W <- read.csv("C:/Users/Arjun/Desktop/topic modelling/w.csv")

ggplot(W, 
       aes(x = w, 
           y = X)) +
  geom_bar(stat = "identity")
#negative wordcloud
neg.tdm = dtm[,which(colnames(dtm) %in% negative_words) ]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")
