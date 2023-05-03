# Take dataframe, split using K-Fold, & return test/train pair.
library("caret")

get_split_data <- function(dataframe) {
    set.seed(232)
    rand <- sample(1:nrow(dataframe),nrow(dataframe)*0.8, replace = FALSE) # 
    train <- dataframe[rand, ]
    test <- dataframe[-rand, ] #selects the other 0.2 indices

    train_control <- trainControl(method="cv", number=10) 

    return(list(train, test, train_control))
}