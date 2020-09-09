library(readxl)
library(lubridate)
library(bReeze)
conv_data <- read.csv("E:/topic modelling/structured_data/structured data (91 days).csv")
conv_data$date <- as.Date(format(conv_data$Timestamp, format = "%y:%m:%d"))
conv_data$day <- weekdays(as.Date(conv_data$date,'%d/%m/%Y'))
conv_data$month <- month(as.Date(conv_data$date),label = TRUE,abbr = F)
conv_data <- conv_data[,c(1:2,16:18,3:15)]
#across India
x <- subset(conv_data,conv_data$country.name==" India")
class(x)
library(ggplot2)
#month
ggplot(x, aes(x = factor(month))) +
  geom_bar(stat = "count")

#Date
one <- as.data.frame(table(x$date))
subset_one <- subset(one,one$Freq>500)
ggplot(subset_one, 
       aes(x = Freq, 
           y = Var1)) +
  geom_bar(stat = "identity")

#Region
two <- as.data.frame(table(x$Region))
subset_two <- subset(two,two$Freq>200)
ggplot(subset_two, 
       aes(x = Freq, 
           y = Var1)) +
  geom_bar(stat = "identity")

#Browser
ggplot(x, aes(y = factor(Browser))) +
  geom_bar()
#Country_name
ggplot(x, aes(y = factor(x$country.name))) +
  geom_bar()

#Platform
ggplot(x, aes(y = factor(Platform))) +
  geom_bar()
#Day
ggplot(x, aes(y = factor(day))) +
  geom_bar()

#across globe
library(ggplot2)
y <- subset(conv_data,conv_data$country.name!=" India")
#month
ggplot(y, aes(x = factor(month))) +
  geom_bar()

#Date
one <- as.data.frame(table(y$date))
subset_one <- subset(one,one$Freq>300)
ggplot(subset_one, 
       aes(x = Freq, 
           y = Var1)) +
  geom_bar(stat = "identity")

#Region
two <- as.data.frame(table(y$Region))
subset_two <- subset(two,two$Freq>150)
ggplot(subset_two, 
       aes(x = Freq, 
           y = Var1)) +
  geom_bar(stat = "identity")

#Browser
ggplot(y, aes(y = factor(Browser))) +
  geom_bar()
#Country_name
three <- as.data.frame(table(y$country.name))
subset_three <- subset(three,three$Freq>150)
ggplot(subset_three, 
       aes(x = Freq, 
           y = Var1)) +
  geom_bar(stat = "identity")

#Platform
ggplot(y, aes(y = factor(Platform))) +
  geom_bar()
#Day
ggplot(y, aes(y = factor(day))) +
  geom_bar()
