faltoons <- read.csv('E:/excelr data/assignments/hypothesis testing/Faltoons.csv')

new <- function(x){
  return(ifelse(x=='Male',1,0))
}
new_data <- as.data.frame(lapply(faltoons,new))
stack_faltoons <- stack(new_data)
attach(stack_faltoons)
table(ind,values)
prop.test(x=c(233,167),n=c(520,280),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
prop.test(x=c(233,167),n=c(520,280),conf.level = 0.95,correct = FALSE,alternative = "greater")
