library(keras)

####데이터 전처리 단계 ####
max_features<-2000 #특징들로 간주할 단어 수

imdb<-dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test,y_test))%<-% imdb

maxlen<-500 #문장당 500단어까지만 사용
input_train<-pad_sequences(input_train,maxlen = maxlen)

input_test<-pad_sequences(input_test,maxlen = maxlen)

#### 모델 구축 ####
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  layer_gru(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

#### callbacks 옵션 1_조기중단, 2_가중치 저장 , 3_결과값 csv 파일로 저장####

callback_list<- list(
  callback_early_stopping(
    monitor = "acc",
    patience = 1
  ),
  callback_model_checkpoint(
    filepath = "my_model.h5",
    monitor = "val_loss",
    save_best_only = TRUE
  ),
  callback_csv_logger(filename = "gru_log.csv",separator = ',')
)

#### callbacks 옵션 4_학습속도 조절 ####
callback_list<- list(
  callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = 0.1,
    patience = 10
  )
)
#### 컴파일 ####
model %>% compile(
  optimizer = "rmsprop", 
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

#### 모델 학습 ####
model %>% fit(input_train, y_train,
              epochs = 10,
              batch_size = 32,
              callbacks = callback_list,
              validation_data = list(input_test,y_test))


