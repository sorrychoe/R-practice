#data setting
load(url('https://github.com/hbchoi/SampleData/raw/master/adult.RData'))

##data preparation
#seperate data to train and test
set.seed(2020)
n_sample <- nrow(adult)
rgroup <- runif(n_sample)

adult.train <- subset(adult, rgroup <= 0.8)
adult.test <- subset(adult, rgroup > 0.8)

#dimention of data
dim(adult.train)
dim(adult.test)

#check of table of train data
table(adult.train$income_mt_50k)

prop.table(table(adult.train$income_mt_50k))
prop.table(table(adult.test$income_mt_50k))

#set the model for analysis 
tble <- table(adult.train$occupation, 
              adult.train$income_mt_50k)
prop.table(tble, margin = 1)

sv_model_job <- prop.table(tble, margin = 1)[ ,2]
sort(sv_model_job, decreasing = T)

#input model to train data
adult.train$est_prob <- sv_model_job[adult.train$occupation]
head(adult.train[ , c('occupation', 'est_prob', 'income_mt_50k')], 10)

#set the threshold and prediction model
#input it to train data
threshold <- 0.4
adult.train$prediction <- adult.train$est_prob > threshold
head(adult.train[, c('occupation', 'est_prob', 'prediction', 'income_mt_50k')], 10)

#check the accuracy
conf.table <- table(pred = adult.train$prediction, 
                    actual = adult.train$income_mt_50k)

accuracy <- sum(diag(conf.table)) / sum(conf.table)
accuracy


#predicion on test data
adult.test$est_prob <- sv_model_job[adult.test$occupation]
adult.test$prediction <- adult.test$est_prob > threshold
head(adult.test[, c('occupation', 'est_prob', 'prediction', 
                    'income_mt_50k')], 10)

conf.table <- table(pred = adult.test$prediction, 
                    actual = adult.test$income_mt_50k)

accuracy <- sum(diag(conf.table)) / sum(conf.table)


#definite function of get accuracy
get_accuracy <- function(pred, actual){
  tble <- table(pred, actual)
  return(round(sum(diag(tble)) / sum(tble), 3))
}

#change threshold and prediction
threshold <- 0.3
adult.train$prediction <- adult.train$est_prob > threshold

print(paste("accuracy on training set", 
            get_accuracy(adult.train$prediction, adult.train$income_mt_50k)))

adult.test$prediction <- adult.test$est_prob > threshold

print(paste("accuracy on test set", 
            get_accuracy(adult.test$prediction, adult.test$income_mt_50k)))


install.packages("ROCR")
library(ROCR)
plot(performance(prediction(adult.test$est_prob, adult.test$income_mt_50k),
                 'tpr', 'fpr'))

calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

calAUC(adult.train$est_prob, adult.train$income_mt_50k)
calAUC(adult.test$est_prob, adult.test$income_mt_50k)
