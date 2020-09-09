library(readr)
library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)

cutlets <- read.csv('E:/excelr data/assignments/hypothesis testing/Cutlets.csv')
View(cutlets)
attach(cutlets)
#....................Normslity test.................#
shapiro.test(Unit.A)
shapiro.test(Unit.B)
#...................Variance test...................#
var.test(Unit.A,Unit.B)
#................... sample T test..................#
t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level=0.95,correct=TRUE)
