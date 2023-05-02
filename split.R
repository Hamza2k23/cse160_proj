# Take dataframe, split using K-Fold, & return test/train pair.
get_split_data <- function(dataframe) {
    set.seed(232)
    rand <- sample(1:nrow(background),nrow(background)*0.8, replace = FALSE) # 
    train <- background[rand, ]
    test <- background[-rand, ] #selects the other 0.2 indices

    train_control <- trainControl(method="cv", number=10) 

    return(train, test, train_control)
}