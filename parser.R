library(readr)
library(stringr)
library(dplyr)
new <- readLines("C:/Users/Arjun/Desktop/topic modelling/newfile.txt")
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
getwd()
setwd("C:/Users/Arjun/Desktop/topic modelling")
write.csv(str_data,"structured data")

#removing timestamp
library(tm)
#library(qdap)
#install.packages("textclean")
#library(textclean
getwd()
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

write_lines(fourteen,"unstructured.txt")
