load(url('https://github.com/hbchoi/SampleData/blob/master/dtree_data.RData?raw=true'))

library(rpart)

loan_model <- rpart(outcome ~ loan_amount + credit_score,  
                    data = loans, method = "class", 
                    control = rpart.control(cp = 0))

#Examine the loan_model object
loan_model

#load the rpart.plot package
library(rpart.plot)

#plot the loan_model with default settings
rpart.plot(loan_model)

#plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"),
           fallen.leaves = TRUE)

##data analysis
#data preparation
loan_model <- rpart(outcome ~ .,  
                    data = loans_train, method = "class", 
                    control = rpart.control(cp = 0))

#make preditions on the training dataset
loans_train$pred <- predict(loan_model, loans_train, 
                            type = 'class')
table(loans_train$outcome, loans_train$pred)

#compute the accuracy on the training dataset
mean(loans_train$outcome == loans_train$pred)

#make predictions on the test dataset
loans_test$pred <- predict(loan_model, loans_test, 
                           type = 'class')
#examine the confusion matrix
table(loans_test$outcome, loans_test$pred)

#compute the accuracy on the test dataset
mean(loans_test$outcome == loans_test$pred)

#delete prediction columns
loans_train <- loans_train[-15]
loans_test <- loans_test[-15]

##pre-pruning exercise
# Grow a tree with maxdepth of 6
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class",
                    control = rpart.control(cp = 0, maxdepth = 6))

# Compute the accuracy of the simpler tree
loans_test$pred <- predict(loan_model, loans_test, type = 'class')
mean(loans_test$outcome == loans_test$pred)

# Grow a tree with minsplit of 500
loan_model2 <- rpart(outcome ~ ., data = loans_train, method =
                       "class", control = rpart.control(cp = 0, minsplit = 500))

# Compute the accuracy of the simpler tree
loans_test$pred2 <- predict(loan_model2, loans_test, type = 'class')
mean(loans_test$outcome == loans_test$pred2)

##post-pruning exercise
# Grow an overly complex tree
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control =
                      rpart.control(cp = 0))
# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned, loans_test, type = 'class')
mean(loans_test$outcome == loans_test$pred)
