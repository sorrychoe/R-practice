#dummy coding
sample_df <- data.frame(blood_type = c('A', 'B','A','O', 'AB'),
                        skin_colour = c('black', 'white', 'yellow', 'red', 'black'), 
                        age = c(22,35,21,26,70))

library(caret)
predict(dummyVars(~ blood_type+skin_colour, data = sample_df), 
        sample_df)
