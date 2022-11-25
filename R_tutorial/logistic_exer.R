load(url('https://github.com/hbchoi/SampleData/raw/master/NatalRiskData.rData'))

train <- sdata[sdata$ORIGRANDGROUP <= 5, ]
test <- sdata[sdata$ORIGRANDGROUP > 5, ]

# making formula for logistic regression model
complications <- c("ULD_MECO", "ULD_PRECIP", "ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT","UPREVIS","CIG_REC","GESTREC3", "DPLURAL", complications,
       riskfactors)
fmla <- paste(y, paste(x, collapse = '+'), sep='~')

print(fmla)

# building logistic regression model
model <- glm(fmla, data = train, family = binomial(link='logit'))

train$pred <- predict(model, newdata = train, type = 'response')
test$pred <- predict(model, newdata = test, type = 'response')

test[20:40, c('pred', 'atRisk')]

aggregate(pred ~ atRisk, data = train, mean)
aggregate(pred ~ atRisk, data = test, mean)

library(ggplot2)
ggplot(train, aes(x = pred, color = atRisk, linetype = atRisk)) + 
  geom_density()

coefficients(model)
