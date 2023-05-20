library(tidyverse)
library(tidytext)
library(wordcloud2)
library(glue)
library(topicmodels)
library(topicdoc)

files <- list.files('./R_test/bible_textmining/NIV_English_Bible/')

num <- data.frame(c(1:150))
colnames(num) <- "word"
num[, 1] <- as.character(num$word)

bibles_words <- data.frame()

gathertext <- function(file){
  fileName <- glue('./R_test/bible_textmining/NIV_English_Bible/', file, sep="")
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

bibles_dtm <- bibles %>%
  count(word, book) %>%
  cast_dtm(book, word, n)

coherence_v <- vector("numeric", 20) 

result <- FindTopicsNumber(
  bibles_dtm,
  topics = seq(from = 10, to = 25, by = 1),
  metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",
  control = list(seed = 1234),
)

FindTopicsNumber_plot(result)

ctm <- CTM(bibles_dtm, k = 18, control = list(seed=1234))

beta_plot(ctm, n = 10)
