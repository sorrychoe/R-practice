load(url('https://github.com/hbchoi/SampleData/raw/master/insurance.RData'))

str(insurance)
summary(insurance)

hist(insurance$charges)
plot(density(insurance$charges))

insurance$charges_log <- log10(insurance$charges)

hist(insurance$charges_log)
plot(density(insurance$charges_log))

set.seed(2018)
ncustomer <- nrow(insurance)
rgroup <- runif(ncustomer)

train.df <- subset(insurance, rgroup <= 0.8)
test.df <- subset(insurance, rgroup > 0.8)

dim(train.df)
dim(test.df)

sv_reg_smoker <- tapply(train.df$charges_log, 
                        train.df$smoker, mean)
sv_reg_smoker

train.df$pred_charges_log <- sv_reg_smoker[train.df$smoker]

head(train.df[, c('smoker', 'pred_charges_log',
                  'charges_log', 'charges')])

train.df$error <- train.df$charges_log - train.df$pred_charges_log

head(train.df[, c('smoker', 'pred_charges_log', 'charges_log', 'error')])

summary(train.df$error)

MSE_train <- mean(train.df$error ** 2)
MSE_train

RMSE_train <- sqrt(MSE_train)
RMSE_train


test.df$pred_charges_log <- sv_reg_smoker[test.df$smoker]

RMSE_test <- sqrt(mean((test.df$charge_log 
                        - test.df$pred_charges_log)**2))
RMSE_test

sd(train.df$charges_log)
sd(test.df$charges_log)


RSS <- sum(train.df$error ** 2)
RSS

SStot <- sum((train.df$charges_log 
              - mean(train.df$charges_log))** 2)
SStot

Rsq <- 1 - RSS/SStot
Rsq



test.df$error <- test.df$charges_log - 
  test.df$pred_charges_log
RSS <- sum(test.df$error ** 2)
RSS

SStot <- sum((test.df$charges_log- 
                mean(test.df$charges_log)) ** 2)
SStot

Rsq <- 1 - RSS/SStot
Rsq

