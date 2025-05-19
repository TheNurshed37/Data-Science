myDatasheet<- read.csv("/Users/bahauddinsakib/Downloads/Diabetes_Prediction.csv",header=TRUE,sep=",")
myDatasheet

summary(myDatasheet)

no_of_col <- ncol(myDatasheet)
no_of_row <- nrow(myDatasheet)
cat("Number of row in the dataset: ", no_of_row) 
cat("Number of column in the dataset: ", no_of_col) 
str(myDatasheet) 

sum(is.na(myDatasheet))

colSums(is.na(myDatasheet))


install.packages("ggplot2")   
library(ggplot2)
install.packages("naniar")   
library(naniar)

gg_miss_upset(myDatasheet)

myDatasheet$age[is.na(myDatasheet$age)] <- mean(myDatasheet$age, na.rm = TRUE)


myDatasheet$blood_glucose_level <- gsub("[^0-9.]", "", myDatasheet$blood_glucose_level)

myDatasheet$blood_glucose_level <- as.numeric(myDatasheet$blood_glucose_level)

myDatasheet$blood_glucose_level[is.na(myDatasheet$blood_glucose_level)] <- mean(myDatasheet$blood_glucose_level, na.rm = TRUE)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

myDatasheet$gender[is.na(myDatasheet$gender)] <- get_mode(myDatasheet$gender)

myDatasheet$smoking_history[is.na(myDatasheet$smoking_history)] <- get_mode(myDatasheet$smoking_history)
myDatasheet <- myDatasheet[myDatasheet$smoking_history != "", ]

myDatasheet$gender[myDatasheet$gender == "Femalee"] <- "Female"
myDatasheet$gender[myDatasheet$gender == "Malee"] <- "Male"

remove<- na.omit(myDatasheet)

myDatasheet$bmi <- as.numeric(myDatasheet$bmi)
myDatasheet$bmi[myDatasheet$bmi <= 0] <- NA

myDatasheet$bmi[is.na(myDatasheet$bmi)] <- mean(myDatasheet$bmi, na.rm = TRUE)

myDatasheet$hypertension[is.na(myDatasheet$hypertension)] <- get_mode(myDatasheet$hypertension)

View(myDatasheet)
sum(is.na(myDatasheet))

gg_miss_var(myDatasheet)
View(myDatasheet)


Q1 <- quantile(myDatasheet$age, 0.25)
Q3 <- quantile(myDatasheet$age, 0.75)
IQR <- Q3 - Q1

myDatasheet <- myDatasheet[!(myDatasheet$age < (Q1 - 1.5 * IQR) | myDatasheet$age > (Q3 + 1.5 * IQR)), ]
View(myDatasheet)

myDatasheet$bmi <- cut(myDatasheet$bmi, breaks = c(0,18.5,24.9,Inf), labels = c("Low", "Medium", "High"))
View(myDatasheet)

myDatasheet$age <- (myDatasheet$age - min(myDatasheet$age)) / (max(myDatasheet$age) - min(myDatasheet$age))


myDatasheet <- myDatasheet[!duplicated(myDatasheet),]
View(myDatasheet)

install.packages("dplyr")
library(dplyr)


filtered_data <- myDatasheet[myDatasheet$age > 50, ]
View(filtered_data)

filtered_data <- myDatasheet %>% filter(age > 50 & gender == "Male")
View(filtered_data)

invalid_age <- myDatasheet$age < 0
print(myDatasheet[invalid_age, ])
View(myDatasheet)

table(myDatasheet$heart_disease)

library(dplyr)

minority_class <- myDatasheet %>% filter(heart_disease == 1)
majority_class <- myDatasheet %>% filter(heart_disease == 0)

set.seed(123)
majority_class_sampled <- majority_class %>% sample_n(nrow(minority_class))

balanced_dataset <- bind_rows(minority_class, majority_class_sampled)

table(balanced_dataset$heart_disease)


n_majority <- nrow(majority_class)
n_minority <- nrow(minority_class)

if(n_majority >= n_minority) {
  majority_class_sampled <- majority_class %>% sample_n(n_minority)
} else {
  print("The majority class is smaller than the minority class.")
}

minority_class_oversampled <- minority_class %>% sample_n(nrow(majority_class), replace = TRUE)

balanced_dataset_oversampled <- bind_rows(minority_class_oversampled, majority_class)

table(balanced_dataset_oversampled$heart_disease)

View(balanced_dataset_oversampled$heart_disease)

set.seed(123)
train_index <- sample(1:nrow(myDatasheet), 0.8 * nrow(myDatasheet))
train_data <- myDatasheet[train_index, ]
test_data <- myDatasheet[-train_index, ]
View(myDatasheet)

mean_age_gender <- aggregate(age ~ gender, data = myDatasheet, FUN = mean)
median_age_gender <- aggregate(age ~ gender, data = myDatasheet, FUN = median)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_age_gender <- aggregate(age ~ gender, data = myDatasheet, FUN = get_mode)
print("Mean Age by Gender:")
print(mean_age_gender)

print("Median Age by Gender:")
print(median_age_gender)

print("Mode Age by Gender:")
print(mode_age_gender)


mean_age_hypertension <- aggregate(age ~ hypertension, data = myDatasheet, FUN = mean)
median_age_hypertension <- aggregate(age ~ hypertension, data = myDatasheet, FUN = median)
mode_age_hypertension <- aggregate(age ~ hypertension, data = myDatasheet, FUN = get_mode) 
print("Mean Age by Hypertension:")
print(mean_age_hypertension)

print("Median Age by Hypertension:")
print(median_age_hypertension)

print("Mode Age by Hypertension:")
print(mode_age_hypertension)


age_summary <- myDatasheet %>%
  group_by(gender) %>%
  summarise(
    Range = max(age) - min(age),
    IQR = IQR(age),
    Variance = var(age),
    Std_Dev = sd(age)
  )

print(age_summary)
View(myDatasheet)


write.csv(myDatasheet, "~/Desktop/My1/myDatasheet_export.csv", row.names = FALSE)











