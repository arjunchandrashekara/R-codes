library(readr)
COF <- read.csv('E:/excelr data/assignments/hypothesis testing/Costomer+OrderForm.csv')
View(COF)
norm_data <- function(x){
  return(ifelse(x=='Error Free',1,0))
}
new_COF <-as.data.frame(apply(COF,MARGIN = 2,norm_data))
stacked_data_COF <- stack(new_COF)
View(stacked_data_COF)
attach(stacked_data_COF)
table(values,ind)
chisq.test(table(values,ind))
