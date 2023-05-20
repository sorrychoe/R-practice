#library setting
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(textdata)
library(glue)
library(stringr)
library(reshape2)

#Bible's tf-idf
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

bibles_tf <- bibles_words %>%
  count(word, sort = TRUE)

bibles_tfidf <- left_join(bibles_words, bibles_tf)

bibles_tfidf <- bibles_tfidf[!duplicated(bibles_tfidf[,c(1,2)]),]

bibles_tfidf <- bibles_tfidf %>%
  anti_join(num) %>%
  bind_tf_idf(word, book, n)

bibles_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  filter(book == "01-Genesis.txt") %>%
  head(25) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(fill = 'blue4', show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

which(bibles_words$word == "matthew")

old_test <- subset(bibles_words[1:596051, ])
new_test <- subset(bibles_words[596052:786713, ])

bibles_tf_old <- old_test %>%
  count(word, sort = TRUE)

bibles_tf_new <- new_test %>%
  count(word, sort = TRUE)

left_join(old_test, bibles_tf_old) -> old_test
left_join(new_test, bibles_tf_new) -> new_test

old <- c("old testment")
new <- c("new testment")

cbind(old_test, part = old) -> old_test
cbind(new_test, part = new) -> new_test

old_test <- old_test[!duplicated(old_test[,c(1,2)]),]
new_test <- new_test[!duplicated(new_test[,c(1,2)]),]

rbind(old_test, new_test) -> testment_tfidf 

testment_tfidf <- testment_tfidf%>%
  bind_tf_idf(word, book, n)

testment_tfidf %>%
  group_by(part) %>%
  slice_max(tf_idf, n=200) %>%
  ggplot(aes(x = tf_idf, y =reorder(word, tf_idf) , fill = part)) +
  geom_col(show.legend = F) +
  facet_wrap(~part, scales = "free")
