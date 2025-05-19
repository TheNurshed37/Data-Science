library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)
library(corrplot)

insurance <- read.csv("D:/STUDY - AIUB/Semester 10/Data Science/final/Lab1/insurance.csv", stringsAsFactors = TRUE)
insurance
View(insurance)


num_data <- insurance %>% select(age, bmi, children, charges)
cor_pearson <- cor(num_data, method = "pearson")
print(cor_pearson)
corrplot(cor_pearson, method = "circle", type = "upper", title = "Pearson Correlation", mar = c(0,0,2,0))

cor_spearman <- cor(num_data, method = "spearman")
print(cor_spearman)
corrplot(cor_spearman, method = "number", type = "upper", title = "Spearman Correlation", mar = c(0,0,2,0))


anova_sex <- aov(charges ~ sex, data = insurance)
print(summary(anova_sex))

anova_region <- aov(charges ~ region, data = insurance)
print(summary(anova_region))

anova_smoker <- aov(charges ~ smoker, data = insurance)
print(summary(anova_smoker))


chisq_sex_smoker <- chisq.test(table(insurance$sex, insurance$smoker))
print(chisq_sex_smoker)

chisq_sex_region <- chisq.test(table(insurance$sex, insurance$region))
print(chisq_sex_region)

chisq_smoker_region <- chisq.test(table(insurance$smoker, insurance$region))
print(chisq_smoker_region)