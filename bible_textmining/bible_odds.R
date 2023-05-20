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

#odds ratio of old/new testment in bible
#get the bible's text data
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

old <- c("old_testment")
new <- c("new_testment")

cbind(old_test, part = old) -> old_test
cbind(new_test, part = new) -> new_test

#rebind old/new testment dataset
rbind(old_test, new_test) -> bibles_set

#calculate odds ratio of old/new testment's text
bibles_odds <- bibles_set %>%
  group_by(part) %>%
  count(word) %>%
  pivot_wider(names_from = part,
              values_from = n, 
              values_fill = list(n=0))%>%
  mutate(odds_old = ((old_testment+1)/sum(old_testment+1)),odds_new = ((new_testment+1)/sum(new_testment+1))) %>% 
  mutate(old_odds_ratio = (odds_old / odds_new), new_odds_ratio = (odds_new / odds_old)) %>%
  filter(rank(old_odds_ratio) <= 30 | rank(new_odds_ratio) <= 30) %>%
  arrange(new_odds_ratio)

#visualization
bibles_odds %>%
  mutate(label = ifelse(new_odds_ratio > 1, "?ž༺??", "???༺??")) %>% 
  mutate(word = reorder(word, old_odds_ratio)) %>%
  ggplot(aes(x = old_odds_ratio, 
             y = word, 
             fill = label)) + 
  geom_col(show.legend = F) +
  facet_wrap(~label, scales = "free")

#log odds ratio for analysis bible's text data
bibles_log_odds <- bibles_set %>%
  group_by(part) %>%
  count(word) %>%
  pivot_wider(names_from = part,
              values_from = n, 
              values_fill = list(n=0))%>%
  mutate(odds_old = ((old_testment+1)/sum(old_testment+1)),odds_new = ((new_testment+1)/sum(new_testment+1))) %>% 
  mutate(log_odds_ratio_new = log(odds_new / odds_old), log_odds_ratio_old = log(odds_old/odds_new)) %>%
  filter(rank(log_odds_ratio_old) <= 20 | rank(log_odds_ratio_new) <= 20) %>%
  arrange(log_odds_ratio_new)

#visualization
bibles_log_odds %>% 
  mutate(label = ifelse(log_odds_ratio_new > 1, "?ž༺??", "???༺??")) %>% 
  ggplot(aes(x = log_odds_ratio_new,
             y = reorder(word, log_odds_ratio_new),
             fill = label)) +
  geom_col()

bibles_log_odds %>% 
  mutate(label = ifelse(log_odds_ratio_old > 1, "???༺??", "?ž༺??")) %>% 
  ggplot(aes(x = log_odds_ratio_old,
             y = reorder(word, log_odds_ratio_old),
             fill = label)) +
  geom_col()


