library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
new <- readLines("E:/topic modelling/merged data set/newfile.txt")
time <- grep(new,pattern = "^Timestamp:",value = T)
#timeframe <- as.data.frame(as.Date(str_extract(time,"\\d{4}-\\d{2}-\\d{2}")))
timetable <- read.table(text=time)
timetable <- as.data.frame(timetable$V2)

response <- grep(new,pattern = "^Unread:",value = T)
responseframe <- as.data.frame(as.character(str_extract(response,"\\b[a-z]+\\b")))

Visitorid <- grep(new,pattern = "^Visitor ID:",value=T)
Visitoridtable <- read.table(text = Visitorid)
visitoridtable <- as.data.frame(Visitoridtable$V3)

emailid <- grep(new,pattern = "^Visitor Email:",value=T)
emailideframe <- as.data.frame(as.character(str_extract(emailid,"\\b[a-z]+\\b")))

Visitornames <-grep(new,pattern = "^Visitor Notes:",value=T) 
Visitornamesframe <- as.data.frame(as.character(str_extract(Visitornames,"\\b[a-z]+\\b")))

IP <- grep(new,pattern = "^IP:",value=T)
IPframe <- as.data.frame(as.factor(str_extract(IP,"\\d+.\\d+.\\d+.\\d+")))

Country <- grep(new,pattern = "^Country Code",value=T)
countryframe <- as.data.frame(as.character(str_extract(Country,"\\b[A-Z]+\\b")))

Countryname <- as.data.frame(grep(new,pattern = "^Country Name",value=T))
colnames(Countryname) <- "Value"

city <- as.data.frame(grep(new,pattern = "^City:",value=T,ignore.case = T))
colnames(city) <- "Value"

Region <-as.data.frame(grep(new,pattern = "^Region:",value=T))
colnames(Region) <- "Value"
Countryname <- str_split_fixed(Countryname$Value,":",2)
Countryname <- as.data.frame(Countryname)
city <- str_split_fixed(city$Value,":",2)
city <- as.data.frame(city)
Region <- str_split_fixed(Region$Value,":",2)
Region <- as.data.frame(Region)
class(Region)
class(city)
class(Countryname)
platform <- as.data.frame(grep(new,pattern = "^Platform:",value=T))

Browser <- as.data.frame(grep(new,pattern = "^Browser:",value=T))

platform <- str_split_fixed(platform$`grep(new, pattern = "^Platform:", value = T)`,":",2)
platform <- as.data.frame(platform)
Browser <- str_split_fixed(Browser$`grep(new, pattern = "^Browser:", value = T)`,":",2)
Browser <- as.data.frame(Browser)

Useragent <- as.data.frame(grep(new,pattern = "^User Agent:",value=T)) 
Useragent <-str_split_fixed(Useragent$`grep(new, pattern = "^User Agent:", value = T)`,":",2) 
Useragent <- as.data.frame(Useragent)
Visitornote <- as.data.frame(grep(new,pattern = "^Visitor Notes:",value=T))
Visitornote <- str_split_fixed(Visitornote$`grep(new, pattern = "^Visitor Notes:", value = T)`,":",2)
Visitornote <- as.data.frame(Visitornote)
city <- as.data.frame(city[-length(city),])

str_data <- data.frame(timetable,responseframe,visitoridtable,Visitornamesframe,Visitornote$V2,emailideframe,IPframe,countryframe,Countryname$V2,Region$V2,city$V2,platform$V2,Browser$V2,Useragent$V2)
colnames(str_data) <- c("Timestamp","Response","Visitor Id","Visitor Email","Visitor name","Visitornote","IP","country code","country name","Region","city","Platform","Browser","Useragent")
setwd("C:/Users/Arjun/Desktop")
write.csv(str_data,"simp.csv")


#removing all the structured data
one <- grep(new,pattern = "^Timestamp:",value = T,invert=T)
two <- grep(one,pattern = "^Unread:",value = T,invert=T)
three <- grep(two,new,pattern = "^Visitor ID:",value=T,invert=T)
four <- grep(three,pattern = "^Visitor Name:",value=T,invert=T)
five <- grep(four,pattern="^Visitor Email:",value=T,invert=T)
six <- grep(five,pattern="^Visitor Notes:",value=T,invert=T)
seven <- grep(six,pattern="^IP:",value=T,invert=T)
eight <- grep(seven,pattern="^Country Code:",value=T,invert=T)
nine <-  grep(eight,pattern="^Country Name:",value=T,invert=T)
ten <-grep(nine,pattern="^Region:",value=T,invert=T)
eleven <- grep(ten,pattern="^City:",value=T,invert=T)
twelve <- grep(eleven,pattern="^User Agent:",value=T,invert=T)
thirteen <- grep(twelve,pattern="^Platform:",value=T,invert=T)
fourteen <- grep(thirteen,pattern="^Browser:",value=T,invert=T)

getwd()
writeLines(fourteen,"simp.txt")

text_data <- read_lines("E:/topic modelling/unstructured_data/unstructured.txt")
fifteen <- grep(text_data,pattern = "^Exclusive October Month Offer :-",value = T,invert = T)
sixteen <- grep(fifteen,pattern = "^Exclusive January Month Offer :-",value = T,invert = T)
seventeen <- grep(sixteen,pattern = "^Exclusive November Month Offer :-",value = T,invert = T)
eighteen <- grep(seventeen,pattern = "^Exclusive December Month Offer :-",value = T,invert = T)
ninteen <- grep(eighteen,pattern = "For this we request you to provide us your Name,Contact number, Email id & Location. So, you will get a quick information about the training",value = T,invert = T)
twenty <- grep(ninteen,pattern = "Mounica Patel: Thank you for connecting with ExcelR! We donâ???Tt just train, we transform by making a POSITIVE impact on your CAREER!",value = T,invert=T)
twenty_one <- grep(twenty,pattern = "Ananya: Thank you for connecting with ExcelR! We donâ???Tt just train, we transform by making a POSITIVE impact on your CAREER!",value = T,invert=T)
write_lines(twenty_one,"final.txt")

# using tm library
library(tm)
text_data <- readLines("C:/Users/Arjun/Desktop/final.txt")
mydata.corpus <- Corpus(VectorSource(text_data))
mydata.corpus <- tm_map(mydata.corpus,tolower)
inspect(mydata.corpus[1:10])
mydata.corpus <- tm_map(mydata.corpus,removePunctuation)
inspect(mydata.corpus[1:5])
mydata.corpus <- tm_map(mydata.corpus,removeNumbers)
inspect(mydata.corpus[1:5])
stopwords <- readLines("E:/excelr data/textmining/stop.txt")
mydata.corpus <- tm_map(mydata.corpus,removeWords,stopwords)
inspect(mydata.corpus[1:5])
mydata.corpus <- tm_map(mydata.corpus,stripWhitespace)
inspect(mydata.corpus[1:5])
mydata.corpus <- tm_map(mydata.corpus,removeWords,stopwords("english"))
inspect(mydata.corpus[1:5])

#creation of documenttermmatrix
dtm.mydata <- DocumentTermMatrix(mydata.corpus)
review_dtm = removeSparseTerms(dtm.mydata, 0.99)
dtm.mydata <- as.matrix(review_dtm)
dim(dtm.mydata)


row_totals <- apply(dtm.mydata,1,sum)
dtm.new <- dtm.mydata[row_totals>0,]
class(dtm.new)
dim(dtm.new)

dtm <- DocumentTermMatrix(mydata.corpus)
dtm1 <- removeSparseTerms(dtm, 0.999)
rt <- apply(dtm1,1,sum)
dtm2 <- dtm1[rt>0,]
dim(dtm2)

#Wordcloud
library(wordcloud)
freq <- colSums(as.matrix(dtm.new))
head(freq,20)
   
pal <- brewer.pal(8,"Dark2")
wordcloud(names(freq), freq, min.freq=6000,colors = pal)
getwd()

#barplot
w <- subset(freq,freq>5000)
w <- as.data.frame(w)
write.csv(w,"w.csv")
W <- read.csv("C:/Users/Arjun/Desktop/w.csv")

ggplot(W, 
       aes(x = w, 
           y = X)) +
  geom_bar(stat = "identity")

library(rvest)
library(XML)
library(magrittr)
library(scales)
library(ggplot2)
library(syuzhet)
library(reshape2)
library(lubridate)

# sentimental analysis
senti_score <- get_nrc_sentiment(as.character(mydata.corpus))
head(senti_score)
#barplots of setimental analysis
barplot(colSums(senti_score),las=2,ylab="count",color=rainbow(10),main="barplots of setimental analysis")

#--------------------------------------------------------#
#   Create Postive Words wordcloud                       #
#--------------------------------------------------------#
positive_words <- readLines("E:/excelr data/textmining/sentimental analysis/positive-words.txt")
pos.tdm = dtm2[,which(colnames(dtm2) %in% positive_words)]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")

#--------------------------------------------------------#
#   Create Negative Words wordcloud                       #
#--------------------------------------------------------#
negative_words <- readLines("E:/excelr data/textmining/sentimental analysis/negative-words.txt")
neg.tdm = dtm2[,which(colnames(dtm2) %in% negative_words) ]
m = as.matrix(neg.tdm)
u = sort(colSums(m), decreasing = TRUE)
wordcloud(names(u), u, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")

#load topic models library
library(topicmodels)
library(tidyverse)
library(tidytext)
library(slam)
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5
#run the LDA model
ldaOut <- LDA(dtm.new,k,control=NULL)
lda.topics <- as.matrix(topics(ldaOut,5))
terms(ldaOut,5)

#Find probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
#investigate topic probabilities data.frame
summary(topicProbabilities)

str(topicProbabilities)
