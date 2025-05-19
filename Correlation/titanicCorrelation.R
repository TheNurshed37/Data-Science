library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)
library(corrplot)

titanic <- read.csv("D:/STUDY - AIUB/Semester 10/Data Science/final/titanic/titanic_train.csv", stringsAsFactors = TRUE)
View(titanic)

titanic <- titanic %>%
  select(-PassengerId, -Name, -Ticket, -Cabin)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)] <- get_mode(titanic$Embarked)


cat("1. ANOVA (Age and Fare vs Survived):\n")
anova_age <- aov(Age ~ Survived, data = titanic)
print(summary(anova_age))
anova_fare <- aov(Fare ~ Survived, data = titanic)
print(summary(anova_fare))


titanic$Survived_num <- as.numeric(as.character(titanic$Survived))
cat("\n2. Kendall’s Tau correlation with Survived:\n")
kendall_age <- cor.test(titanic$Age, titanic$Survived_num, method = "kendall")
kendall_fare <- cor.test(titanic$Fare, titanic$Survived_num, method = "kendall")
print(kendall_age)
print(kendall_fare)


cat("\n3. Pearson Correlation (Numeric features):\n")
num_data <- titanic %>% select(Age, Fare, SibSp, Parch)
cor_pearson <- cor(num_data, method = "pearson")
print(cor_pearson)
corrplot(cor_pearson, method = "circle", type = "upper", title = "Pearson Correlation", mar = c(0,0,2,0))


cat("\n4. Spearman Correlation (Numeric features):\n")
cor_spearman <- cor(num_data, method = "spearman")
print(cor_spearman)
corrplot(cor_spearman, method = "number", type = "upper", title = "Spearman Correlation", mar = c(0,0,2,0))