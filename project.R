library(ggplot2)
library(caret)
source("data.R")
source("split.R")

## R Markdown

# retrieves cleaned data with merged gpa 

background <- get_data()

background$household_income
background$relationship_m
background$explain_wrong_m

split_data <- get_split_data(background)
nrow(background$gpa)
train <- split_data[1]
test <- split_data[2]
train_control <- split_data[3] # to be used when building model. refer to use for how use  [https://www.geeksforgeeks.org/k-fold-cross-validation-in-r-programming/#]

model <- train(gpa~ miss_m + miss_f + time_lived_m + time_lived_f + closeness_m + closeness_f + verbal_abuse_o +
                 emotional_exp_o + verbal_abuse_mf + verbal_abuse_f + verbal_abuse_m + treats_other_child_f +
                 treats_other_child_m + explain_wrong_f + explain_wrong_m + relationship_f + relationship_m + household_income, data = background,
               trControl = train_control,
               method = "nb")

pred = predict(model, test, type="raw")
tab <- table(pred, testData$class)
accuracy <- sum(diag(tab))/sum(tab)
accuracy