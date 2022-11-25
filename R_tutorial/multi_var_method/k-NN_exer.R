wbcd <- read.csv("https://github.com/hbchoi/SampleData/raw/master/wisc_bc_data.csv",
                 stringsAsFactors = F)
str(wbcd)

# removing ID variable
wbcd <- wbcd[,-1]

table(wbcd$diagnosis)

# changing value for clear interpretation
wbcd$diagnosis <- ifelse(wbcd$diagnosis == 'B', 'Benign', 'Malignant')

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#normalization
minmax_norm <- function(x){
  (x-min(x))/(max(x) - min(x))
}

wbcd_norm <- sapply(wbcd[,-1], minmax_norm)

summary(wbcd_norm[,c("radius_mean", "area_mean", "smoothness_mean")])

# split data into train and test set
dim(wbcd_norm)

wbcd_train <- wbcd_norm[1:469, ]
wbcd_test <- wbcd_norm[470:569, ]

wbcd_train_label <- wbcd[1:469, 1]
wbcd_test_label <- wbcd[470:569, 1]

# choosing proper k
sqrt(nrow(wbcd_train))

library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_label, k = 21)
wbcd_test_pred

#accuracy
mean(wbcd_test_label == wbcd_test_pred)

#confusion matrix
cmat <- table(wbcd_test_label, wbcd_test_pred)
cmat

#probabilistic interpretation
##wbcd_test_pred is changed##
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_label, k = 21, prob = TRUE)
head(wbcd_test_pred)
head(attributes(wbcd_test_pred)$prob)

wbcd_test_pred_prob <- ifelse(wbcd_test_pred == "Malignant",
                              attributes(wbcd_test_pred)$prob,
                              1-attributes(wbcd_test_pred)$prob)
head(wbcd_test_pred_prob)

#ROC curve and AUC
library(ROCR)
plot(performance(prediction(wbcd_test_pred_prob, wbcd_test_label == 'Malignant'),
                 'tpr', 'fpr'))

calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

calAUC(wbcd_test_pred_prob, wbcd_test_label == 'Malignant')

#set lower threshold
threshold <- 0.1
wbcd_test_pred_new <- ifelse(wbcd_test_pred_prob > threshold,
                             'Malignent', 'Benign')
##cmat is changed ##
cmat <- table(wbcd_test_label, wbcd_test_pred_new)
cmat

#accuracy --> result is different... why?
mean(wbcd_test_label == wbcd_test_pred_new)

#precision
cmat[2,2]/sum(cmat[ ,2])

#recall
cmat[2,2]/sum(cmat[2,])
