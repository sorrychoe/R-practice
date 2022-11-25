load(url('https://github.com/hbchoi/SampleData/raw/master/nb_data.RData'))

head(locations)
str(locations)

head(where9am)


#simple naive bayes model
library(naivebayes)

locmodel <- naive_bayes(location ~ daytype, data = where9am) 
locmodel

#make prediction
test_simple

predict(locmodel, newdata = test_simple)
predict(locmodel, newdata = test_simple, type = 'prob')

#more sophisticated location model
locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations_train)
pred <- predict(locmodel, locations_test)

mean(locations_test$location == pred)

locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations_train)
pred <- predict(locmodel, locations_test, type = 'prob')
head(pred, n = 4)

# laplace corrected model
locmodel_lap <- naive_bayes(location ~ daytype + hourtype, data = locations_train, laplace = 1)
pred <- predict(locmodel_lap, locations_test, type = 'prob')
head(pred, n = 4)
