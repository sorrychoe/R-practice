#modeling
load(url('https://github.com/hbchoi/SampleData/raw/master/adult.RData'))

set.seed(2020)
n_sample <- nrow(adult)
rgroup <- runif(n_sample)

adult.train <- subset(adult, rgroup <= 0.8)
adult.test <- subset(adult, rgroup > 0.8)

summary(adult$age)
adult.train$age_group <- cut(adult.train$age, breaks = c(0,20,30,40,50,60,Inf),
                             labels = c('under20', '20s', '30s', '40s', '50s', 'over60'),
                             right = F)
table(adult.train$age_group)
