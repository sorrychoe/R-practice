library(tidyverse)
library(tidytext)
library(bitTA)
library(wordcloud2)
library(glue)
library(topicmodels)
library(topicdoc)

#Bible's tf-idf
files <- list.files('C:/Users/cjsso/Downloads/day3_text_mining/NIV_English_Bible/')

num <- data.frame(c(1:150))
colnames(num) <- "word"
num[, 1] <- as.character(num$word)

bibles_words <- data.frame()

gathertext <- function(file){
  fileName <- glue('C:/Users/cjsso/Downloads/day3_text_mining/NIV_English_Bible/', file, sep="")
  filetext <- readLines(fileName)
  tokens <- tibble(text=filetext) %>%
    unnest_tokens(word,text) %>%
    mutate(book = file)
}

for(i in files){
  bibles_words <- bibles_words %>%
    rbind(gathertext(i))
}

bibles <-bibles_words %>%
  anti_join(num)

## for make the dtm, bibles_words get a term count 
bibles_dtm <- bibles %>%
  count(word, book) %>%
  cast_dtm(book, word, n)

loglik_v     <- vector("numeric", 20) 
perplexity_v <- vector("numeric", 20) 

for (i in 2:20) {
  cat("... ", i, "\n")
  tmp_mod  <- LDA(bibles_dtm, k=i, method="Gibbs", control=list(alpha=0.5, iter=1000, seed=1234, thin=3))
  loglik_v[i] <- logLik(tmp_mod)
  perplexity_v[i] <- perplexity(tmp_mod, newdata =  bibles_dtm)
}

topic_k_df <- tibble(
  topic_k = 1:20,
  loglik = loglik_v,
  perplexity = perplexity_v)

topic_k_df %>%
  ggplot(aes(x=topic_k, y=perplexity)) +
  geom_line()+
  geom_point()

topic_k_df %>%
  ggplot(aes(x=topic_k, y=loglik)) +
  geom_line()+
  geom_point()

lda <- LDA(bibles, k = 5, control = list(seed=1234))
lda


