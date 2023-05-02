library(caret)


## R Markdown

# retrieves cleaned data with merged gpa 
background <- get_data()

split_data <- split_data(background)

train <- split_data$train
test <- split_data$test
train_control <- split_data$train_control # to be used when building model. refer to use for how use  [https://www.geeksforgeeks.org/k-fold-cross-validation-in-r-programming/#]




