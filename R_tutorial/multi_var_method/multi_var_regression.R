load(url('https://github.com/hbchoi/SampleData/raw/master/insurance.RData'))

#data preparation
set.seed(2018)
ncustomer <- nrow(insurance)
rgroup <- runif(ncustomer)

# data partition to learn a prediction model
train.df <- subset(insurance, rgroup <= 0.8)

# hold-out data for testing
test.df <- subset(insurance, rgroup > 0.8)

ins_model <- lm(charges ~ ., train.df)
ins_model

train.df$pred <- predict(ins_model, newdata = train.df)
test.df$pred <- predict(ins_model, newdata = test.df)

#set function
calcRMSE <- function(label, estimation){
  return(sqrt(mean((label - estimation) ** 2)))
}
calcR2 <- function(label, estimation){
  RSS = sum((label - estimation) ** 2)
  SStot = sum((label - mean(label)) ** 2)
  return(1-RSS/SStot)
}

#performance on train,df
calcRMSE(train.df$charges, train.df$pred)
calcR2(train.df$charges, train.df$pred)

#performance on test.df
calcRMSE(test.df$charges, test.df$pred)
calcR2(test.df$charges, test.df$pred)

#visualization
library(ggplot2)
ggplot(train.df, aes(x = pred, y = charges)) + 
  geom_point(alpha = 0.2, col = 'black') + 
  geom_smooth() + 
  geom_line(aes(x = charges, y = charges), col = 'blue', linetype = 2)

ggplot(train.df, aes(x = pred, y = pred - charges)) +
  geom_point(alpha = 0.2, col = 'black') +
  geom_smooth()+
  geom_hline(yintercept = 0, col = 'blue', linetype = 2)

#model improvement
train.df$bmi30 <- ifelse(train.df$bmi >= 30, 1, 0)
test.df$bmi30 <- ifelse(test.df$bmi >= 30, 1, 0)

ins_model <- lm(charges ~ age + I(age**2) + sex + bmi + children +
                  bmi30 * smoker + region, train.df)
ins_model

#get prediction
train.df$pred <- predict(ins_model, newdata = train.df)
test.df$pred <- predict(ins_model, newdata = test.df)

#performance on train,df
calcRMSE(train.df$charges, train.df$pred)
calcR2(train.df$charges, train.df$pred)

#performance on test.df
calcRMSE(test.df$charges, test.df$pred)
calcR2(test.df$charges, test.df$pred)

#visualization
library(ggplot2)
ggplot(train.df, aes(x = pred, y = charges)) + 
  geom_point(alpha = 0.2, col = 'black') + 
  geom_smooth() + 
  geom_line(aes(x = charges, y = charges), col = 'blue', linetype = 2)

ggplot(train.df, aes(x = pred, y = pred - charges)) +
  geom_point(alpha = 0.2, col = 'black') +
  geom_smooth()+
  geom_hline(yintercept = 0, col = 'blue', linetype = 2)
