data(iris)
head(iris)

#seed data setting
set.seed(2021)
n.sample <- nrow(iris)
rgroup <- runif(n.sample)

#set train & test data set
iris.train <- subset(iris, rgroup < 0.8)
iris.test <- subset(iris, rgroup >= 0.8)


irisVars <- setdiff(colnames(iris),list('rgroup', 'Species'))
irisFormula <- as.formula(paste('Species=="versicolor"',
                                paste(irisVars,collapse=' + '),sep=' ~ '))

#function for accuracy measure
accuracyMeasures <- function(pred, truth, name="model") {
  ctable <- table(truth=truth,
                  pred=(pred>0.5))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- 2*precision*recall/(precision+recall)
  data.frame(model=name, accuracy=accuracy, f1=f1)
}

library(rpart)
treemodel <- rpart(irisFormula, iris.train)

accuracyMeasures(predict(treemodel, newdata=iris.train),
                 iris.train$Species=="versicolor",
                 name="tree, training")

accuracyMeasures(predict(treemodel, newdata=iris.test),
                 iris.test$Species=="versicolor",
                 name="tree, training")

##bagging
ntrain <- dim(iris.train)[1]
n <- ntrain
ntree <- 100

#set the bootstrap
samples <- sapply(1:ntree,
                  FUN = function(iter)
                  {sample(1:ntrain, size=n, replace=T)})

treelist <-lapply(1:ntree,
                  FUN=function(iter)
                  {samp <- samples[,iter];
                  rpart(irisFormula, iris.train[samp,])})

#set the bagging functuon
predict.bag <- function(treelist, newdata) {
  preds <- sapply(1:length(treelist), 
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)})
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

#check the accuracy
accuracyMeasures(predict.bag(treelist, newdata=iris.train),
                 iris.train$Species=="versicolor",
                 name="bagging, training")

accuracyMeasures(predict.bag(treelist, newdata=iris.test),
                 iris.test$Species=="versicolor",
                 name="bagging, training")


#random forest
library(randomForest)
set.seed(1025)
fmodel <- randomForest(x=iris.train[,irisVars],
                       y=iris.train$Species,
                       ntree = 100,
                       nodesize = 7,
                       importance = T)

accuracyMeasures(predict(fmodel, newdata=iris.train[,irisVars], type = "prob")[, 'Species'],
                 iris.train$Species=="versicolor", name="random Forest, train")
