library(ggplot2)
library(caret)
source("data.R")
source("split.R")
library(iris)

## R Markdown

# retrieves cleaned data with merged gpa 

background <- get_data()

background$household_income
background$relationship_m
background$explain_wrong_m

split_data <- get_split_data(background)

train <- data.frame(split_data[1])
test <- data.frame(split_data[2])

# to be used when building model. refer to use for how use  [https://www.geeksforgeeks.org/k-fold-cross-validation-in-r-programming/#]



model <- train(gpa~ miss_events_m + miss_events_f + time_lived_m + time_lived_f + closeness_m + closeness_f + verbal_abuse_o +
                 emotional_exp_o + verbal_abuse_mf + verbal_abuse_f + verbal_abuse_m + treats_other_child_f +
                 treats_other_child_m + explain_wrong_f + explain_wrong_m + relationship_f + relationship_m + household_income, data = train,
               trControl = trainControl(method = "cv", number = 10), method = "lm")


pred = predict(model, test, type="raw")
pred
tab <- table(pred, test$gpa)
tab
rmse <- sqrt(mean((pred - test$gpa)^2))
rmse
accuracy <- sum(diag(tab))/sum(tab)
accuracy