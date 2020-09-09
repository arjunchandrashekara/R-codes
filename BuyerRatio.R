library(readr)
buyer_ratio <- read.csv('E:/excelr data/assignments/hypothesis testing/BuyerRatio.csv')
View(buyer_ratio)
stacked_buyer_ratio <- stack(buyer_ratio)
View(stacked_buyer_ratio)
attach(stacked_buyer_ratio)
table(values,ind)
chisq.test(table(values,ind))

