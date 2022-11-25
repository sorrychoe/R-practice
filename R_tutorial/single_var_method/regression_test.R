#data loading
load(url('https://github.com/hbchoi/SampleData/raw/master/insurance.RData'))

#data exploration
str(insurance)
summary(insurance)

#set log10 at charges
insurance$charges_log <- log10(insurance$charges)

#data preparation
set.seed(2018)
ncustomer <- nrow(insurance)
rgroup <- runif(ncustomer)

train.df <- subset(insurance, rgroup <= 0.8)
test.df <- subset(insurance, rgroup > 0.8)

#data modeling
sv_reg_region <- tapply(train.df$charges_log, 
                        train.df$region, mean)
sv_reg_region

#set prediction
train.df$pred_charges_log <- sv_reg_region[train.df$region]

head(train.df[, c('region', 'pred_charges_log',
                  'charges_log', 'charges')])

#errors
train.df$error <- train.df$charges_log - train.df$pred_charges_log

head(train.df[, c('region', 'pred_charges_log', 'charges_log', 'error')])

#RMSE
MSE_train <- mean(train.df$error ** 2)
MSE_train

RMSE_train <- sqrt(MSE_train)
RMSE_train

test.df$pred_charges_log <- sv_reg_region[test.df$region]
test.df$error <- test.df$charges_log - 
  test.df$pred_charges_log

MSE_test <- mean(test.df$error ** 2)
MSE_test

RMSE_test <- sqrt(MSE_test)
RMSE_test


#R square for training data
RSS <- sum(train.df$error ** 2)
RSS

SStot <- sum((train.df$charges_log 
              - mean(train.df$charges_log))** 2)
SStot

Rsq <- 1 - RSS/SStot
Rsq

#R square for test data
RSS <- sum(test.df$error ** 2)
RSS

SStot <- sum((test.df$charges_log- 
                mean(test.df$charges_log)) ** 2)
SStot

Rsq <- 1 - RSS/SStot
Rsq






####################################################
#set age variable
sv_reg_age <- tapply(train.df$charges_log, 
                        train.df$age, mean)
sv_reg_age

#prediction
##Why NA data is happen?
train.df$pred_charges_log <- sv_reg_age[train.df$age]

summary(train.df$pred_charges_log)

head(train.df[, c('age', 'pred_charges_log',
                  'charges_log', 'charges')])

#errors
train.df$error <- train.df$charges_log - train.df$pred_charges_log

tail(train.df[, c('age', 'pred_charges_log', 'charges_log', 'error')])

summary(train.df$error)

#RMSE
MSE_train <- mean(train.df$error ** 2)
MSE_train

RMSE_train <- sqrt(MSE_train)
RMSE_train

test.df$pred_charges_log <- sv_reg_age[test.df$age]

RMSE_test <- sqrt(mean((test.df$charge_log 
                        - test.df$pred_charges_log)**2))
RMSE_test

#R square for training data
RSS <- sum(train.df$error ** 2)
RSS

SStot <- sum((train.df$charges_log 
              - mean(train.df$charges_log))** 2)
SStot

Rsq <- 1 - RSS/SStot
Rsq


#R square for test data
test.df$error <- test.df$charges_log - 
  test.df$pred_charges_log
RSS <- sum(test.df$error ** 2)
RSS

SStot <- sum((test.df$charges_log- 
                mean(test.df$charges_log)) ** 2)
SStot

Rsq <- 1 - RSS/SStot
Rsq
