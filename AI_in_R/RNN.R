samples <- c("The cat sat on the mat.", "The dog ate my homework.")

token_index <- list()

for (sample in samples)
  for (word in strsplit(sample, " ")[[1]])
    if (!word %in% names(token_index))
      token_index[[word]] <- length(token_index) + 2 

token_index


max_length <- 10

results <- array(0, dim = c(length(samples), 
                            max_length, 
                            max(as.integer(token_index))))

for (i in 1:length(samples)) {
  sample <- samples[[i]]
  words <- head(strsplit(sample, " ")[[1]], n = max_length)
  for (j in 1:length(words)) {
    index <- token_index[[words[[j]]]]
    results[[i, j, index]] <- 1
  }
}

results[,,2:11]



library(keras)
samples <- c("The cat sat on the mat.", "The dog ate my homework.")

tokenizer <- text_tokenizer(num_words = 1000) %>%
  fit_text_tokenizer(samples)


sequences <- texts_to_sequences(tokenizer, samples)
View(sequences)

one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary")
View(one_hot_results)

word_index <- tokenizer$word_index
cat(length(word_index), "개의 토큰을 찾았습니다.\n")




embedding_layer<-layer_embedding(input_dim = 1000, output_dim = 64)


max_features <- 10000

maxlen <- 20

imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb

x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)


model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = 10000, output_dim = 8, 
                  input_length = maxlen) %>% 
  layer_flatten() %>% 
  layer_dense(units = 1, activation = "sigmoid") 

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2
)
