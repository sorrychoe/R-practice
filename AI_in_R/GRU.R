dir.create("E:/R/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "E:/R/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "E:/R/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "E:/R/jena_climate"
)

library(tibble)
library(readr)
data_dir <- "E:/R/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")

data <- read_csv(fname)

glimpse(data)


library(ggplot2)

#전체 기온 그래프
ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()
# 처음 10일간의 기온 그래프
ggplot(data[1:1440,], aes(x = 1:1440, y = `T (degC)`)) + geom_line()

#먼저 날짜기록을 제거
data <- data.matrix(data[,-1]) 
#정규화 작업
#42만개의 데이터중 20만개 정로를 학습데이터로
train_data <- data[1:200000,]

#정규화
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
#scale()로 정규화
data <- scale(data, center = mean, scale = std)
summary(data)


sequence_generator <- function(start){
  value<-start -1
  function(){
    value <<- value + 1
    value
  }
}

gen_9<-sequence_generator(9)
gen_9()
gen_3<-sequence_generator(3)
gen_3()


generator <- function(data, 
                      lookback, 
                      delay, 
                      min_index, 
                      max_index,
                      shuffle = FALSE, 
                      batch_size = 128, 
                      step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
  }
}
library(keras)
#기본 인자값
lookback <- 1440 # 10일간의 기록
step <- 6 #스텝 수
delay <- 144  # 하루 뒤 예측 = 1440분 
batch_size <- 128 

#학습데이터 생성기 
  train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)
train_gen()

#검증데이터 생성기
val_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)
val_gen()

#테스트 데이터 생성기
test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)
test_gen()

val_steps <- (300000 - 200001 ) / batch_size
test_steps <- (nrow(data) - 300001 ) / batch_size


##기본으로만 문제를 해결할 때
library(keras)
model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

summary(model)


model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("acc")
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
  )


#### 1. gru 모델 ####
library(keras)
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)


history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
  
)



# 재귀적 드롭아웃
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, dropout = 0.15, recurrent_dropout = 0.3,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  #layer_gru(units = 32, dropout = 0.15, recurrent_dropout = 0.3) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 40,                       #에폭수 2배 증가
  validation_data = val_gen,
  validation_steps = val_steps
)



# GRU 계층 2개 적층(다적층)  / units값 2배 증가 
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, 
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE, #재귀신경망을 중첩시킬때 추가시켜야 하는 인자 return_sequences.
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)

