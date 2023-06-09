# Source data from CSV, isolate columns, create dataframe, & manage NA / negative numbers
get_data <- function() {
    background <- read.csv("background.csv")
    #background
    background_containing_gpa <- read.csv("train.csv")
    #merge 
    background <- merge(background, background_containing_gpa, by = "challengeID")

    # replace NA gpa entries with avg
    avg_gpa <- round(mean(background$gpa, na.rm=TRUE), 2)
    gpa <- ifelse(is.na(background$gpa), avg_gpa, background$gpa)

    # income 
    household_income <- background$cm5hhinc  + background$cf5hhinc
    avg_income <- mean(household_income)
    household_income <- ifelse(household_income < 0, avg_income, household_income)

    #relationship mother/father
    avg_relm <- mean(background$m5c1[background$m5c1 >= 0])
    relationship_m <- ifelse(background$m5c1 < 0, avg_relm, background$m5c1)
    avg_relf <- round(mean(background$f5c1[background$f5c1 >= 0]))
    relationship_f <- ifelse(background$f5c1 < 0, avg_relf, background$f5c1)


    # explain wrong
    avg_m <- mean(background$k5b1a[background$k5b1a  >= 0])
    explain_wrong_m <- ifelse(background$k5b1a < 0, avg_m, background$k5b1a)
    avg_f <- mean(background$k5b2a[background$k5b2a  >= 0])
    explain_wrong_f <- ifelse(background$k5b2a  < 0, avg_f, background$k5b2a )

    # treats other child better
    avg_m <- mean(background$k5c2[background$k5c2 > 0])
    treats_other_child_m <- ifelse(background$k5c2  < 0, avg_m, background$k5c2 )
    avg_f <- mean(background$k5c6[background$k5c6 > 0])
    treats_other_child_f <- ifelse(background$k5c6  < 0, avg_f, background$k5c6 )

    # verbal abuse
    avg_m <- mean(background$k5b1c[background$k5b1c >= 0])
    verbal_abuse_m <- ifelse(background$k5b1c  < 0, avg_m, background$k5b1c)
    avg_f <- mean(background$k5b2c[background$k5b2c >= 0])
    verbal_abuse_f <- ifelse(background$k5b2c  < 0, avg_f, background$k5b2c)

    #combining mother and father for observation comparison
    verbal_abuse_mf <-   ((verbal_abuse_f + verbal_abuse_m ) / 2) + 1 # maps 0-4 values to 1-2, useful for comparing observation to parent responses
    verbal_abuse_mf[verbal_abuse_mf > 2] <- 2
    verbal_abuse_mf[verbal_abuse_mf < 1] <- 1

    # positive emotional expression observation
    avg_emo <- mean(background$o5e7[background$o5e7 > 0])
    emotional_exp_o <- ifelse(background$o5e7  < 0, avg_emo, background$o5e7)

    # observation of verbal abuse
    avg_o <- mean(background$o5e8[background$o5e8 > 0])
    verbal_abuse_o <- ifelse(background$o5e8  < 0, avg_o, background$o5e8)

    #closeness to mother and father
    avg <- mean(background$k5a2e[background$k5a2e >= 0])
    closeness_m <- ifelse(background$k5a2e  < 0, avg, background$k5a2e)
    avg <- mean(background$k5a3e[background$k5a3e >= 0])
    closeness_f <- ifelse(background$k5a3e  < 0, avg, background$k5a3e)

    #time lived with mom/dad
    avg <- mean(background$f5a2[background$f5a2 > 0])
    time_lived_f <- ifelse(background$f5a2  < 0, avg, background$f5a2)
    avg <- mean(background$m5a2[background$m5a2 > 0])
    time_lived_m <- ifelse(background$m5a2  < 0, avg, background$m5a2)

    # miss events mom/dad
    avg <- mean(background$k5a4d[background$k5a4d > 0])
    miss_events_f <- ifelse(background$k5a4d  < 0, avg, background$k5a4d)
    avg <- mean(background$k5a2d[background$k5a2d > 0])
    miss_events_m <- ifelse(background$k5a2d  < 0, avg, background$k5a2d)
    
    challengeID <- background$challengeID
    
    df <- data.frame(challengeID, gpa, household_income, relationship_f, relationship_m, 
                     explain_wrong_f, explain_wrong_m, treats_other_child_f, 
                     treats_other_child_m, verbal_abuse_o, verbal_abuse_mf, 
                     verbal_abuse_f, verbal_abuse_m, emotional_exp_o, closeness_m, 
                     closeness_f, time_lived_f, time_lived_m, miss_events_m, miss_events_f)
    return(df)
}

