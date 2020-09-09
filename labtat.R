library(readr)
labtat <- read.csv('E:/excelr data/assignments/hypothesis testing/LabTAT.csv')
stacked_labtat <- stack(labtat)
stacked_labtat$ind <- as.factor(stacked_labtat$ind)


#...........................Normality Test.................................#
attach(labtat)
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)

#........................Variance Test....................................#
bartlett.test(values~ind,data=stacked_labtat)

#........................Anova Test.......................................#
anova_results <- aov(values~ind,data=stacked_labtat)
summary(anova_results)
model <- lm(values~ind,data=stacked_labtat)
anova(model)
TukeyHSD(aov(model))

#......................Sample Mean......................................#

mean(labtat$Laboratory.1)
mean(labtat$Laboratory.2)
mean(labtat$Laboratory.3)
mean(labtat$Laboratory.4)
