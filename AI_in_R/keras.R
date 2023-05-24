#케라스 설치 CPU 버전
install.packages('devtools')
devtools::install_github("rstudio/keras")

#케라스 설치 GPU 버전 
install_keras(tensorflow = "gpu")

library(keras)
install_keras()

# getwd()
# setwd("")


mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images)
str(train_labels)

str(test_images)
str(test_labels)

network <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>% 
  layer_dense(units = 10, activation = "softmax")

summary(network)

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255
test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

metrics <- network %>% evaluate(test_images, test_labels, verbose = 0)
metrics

network<-predict_classes(test_images[1:10,])

#시각화 1
test_images[1,]
displayDigit <- function(X){
  m <- matrix(unlist(X),nrow = 28,byrow = T)
  m <- t(apply(m, 2, rev))
  image(m,col=grey.colors(255))
}
displayDigit(test_images[1,])

#시각화 2
c(c(train_images,train_labels),c(test_images,test_labels)) %<-% mnist
digit<-test_images[1,,]
plot(as.raster(digit,max = 255))