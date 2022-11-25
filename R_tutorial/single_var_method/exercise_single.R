#modeling
load(url('https://github.com/hbchoi/SampleData/raw/master/adult.RData'))

set.seed(2020)
n_sample <- nrow(adult)
rgroup <- runif(n_sample)

adult.train <- subset(adult, rgroup <= 0.8)
adult.test <- subset(adult, rgroup > 0.8)

table(adult.train$income_mt_50k)

prop.table(table(adult.train$income_mt_50k))
prop.table(table(adult.test$income_mt_50k))

tble <- table(adult.train$education, 
              adult.train$income_mt_50k)
prop.table(tble, margin = 1)

model_edu_job <- prop.table(tble, margin = 1)[ ,2]
sort(model_edu_job, decreasing = T)

adult.train$est_prob <- model_edu_job[adult.train$education]
head(adult.train[ , c('education', 'est_prob', 'income_mt_50k')], 10)

threshold <- 0.5
adult.train$prediction <- adult.train$est_prob > threshold
head(adult.train[, c('education', 'est_prob', 'prediction', 'income_mt_50k')], 10)


adult.test$est_prob <- model_edu_job[adult.test$education]
adult.test$prediction <- adult.test$est_prob > threshold
head(adult.test[, c('education', 'est_prob', 'prediction', 
                    'income_mt_50k')], 10)


get_accuracy <- function(pred, actual){
  tble <- table(pred, actual)
  return(round(sum(diag(tble)) / sum(tble), 3))
}

get_accuracy(adult.train$prediction, adult.train$income_mt_50k)
get_accuracy(adult.test$prediction, adult.test$income_mt_50k)


#change threshold to 0.6
threshold <- 0.6
adult.train$prediction <- adult.train$est_prob > threshold
adult.test$prediction <- adult.test$est_prob > threshold

get_accuracy(adult.train$prediction, adult.train$income_mt_50k)
get_accuracy(adult.test$prediction, adult.test$income_mt_50k)


#change threshold to 0.4
threshold <- 0.4
adult.train$prediction <- adult.train$est_prob > threshold
adult.test$prediction <- adult.test$est_prob > threshold

get_accuracy(adult.train$prediction, adult.train$income_mt_50k)
get_accuracy(adult.test$prediction, adult.test$income_mt_50k)

