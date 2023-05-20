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
library(tidylo)

files <- list.files('./R_test/bible_textmining/NIV_English_Bible/')

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

#seperate old/new testment dataset 
old_test <- subset(bibles_words[1:596051, ])
new_test <- subset(bibles_words[596052:786713, ])

bibles_tf_old <- old_test %>%
  count(word, sort = TRUE)

bibles_tf_new <- new_test %>%
  count(word, sort = TRUE)

old_test <- left_join(old_test, bibles_tf_old)
new_test <- left_join(new_test, bibles_tf_new)

old_test <- old_test[!duplicated(old_test[,c(1,2)]),]
new_test <- new_test[!duplicated(new_test[,c(1,2)]),]

old <- c("old_testment")
new <- c("new_testment")

cbind(old_test, part = old) -> old_test
cbind(new_test, part = new) -> new_test

#rebind old/new testment dataset
rbind(old_test, new_test) -> bibles_set

wlor <- bibles_set %>%
  group_by(part) %>%
  bind_log_odds(set = part,
                feature = word,
                n = n) %>% 
  slice_max(log_odds_weighted, n = 10) %>%
  select(part, word, point = log_odds_weighted)

tfidf <- bibles_set %>%
  group_by(part)%>%
  bind_tf_idf(word, book, n) %>%
  slice_max(tf_idf, n = 100) %>%
  select(part, word, point = tf_idf)

odds <- bibles_set %>%
  group_by(part) %>%
  count(word) %>%
  pivot_wider(names_from = part,
              values_from = n, 
              values_fill = list(n=0))%>%
  mutate(odds_old = ((old_testment+1)/sum(old_testment+1)),odds_new = ((new_testment+1)/sum(new_testment+1))) %>% 
  mutate(log_odds_ratio = log(odds_new / odds_old))

odds <- odds %>%
  group_by(part = ifelse(log_odds_ratio > 1, "new_testment", "old_testment")) %>%
  slice_max(log_odds_ratio, n = 10) %>%
  select(part, word, point = log_odds_ratio)

bind_rows(wlor, odds, tfidf, .id = "ID")%>% 
  mutate(ID = ifelse(ID == "1", "???߷α׽»???", ifelse(ID == "2", "?α׽»???", "tf-idf"))) %>%
  ggplot(aes(x = point, 
             y = reorder(word, point),
             fill= part)) +
  geom_col(show.legend = F) +
  facet_wrap(~part + ID, scales = "free")
         