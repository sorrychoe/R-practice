#library install
install.packages("tidylo")

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

#get the bible's text data
files <- list.files('C:/Users/cjsso/Downloads/day3_text_mining/NIV_English_Bible/')

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

num <- data.frame(c(1:150))
colnames(num) <- "word"
num[, 1] <- as.character(num$word)

bibles_set %>%
  anti_join(num) -> bibles_set


bible_odd <- bibles_set%>%
  bind_log_odds(set = part,
                feature = word,
                n = n) %>% 
  arrange(-log_odds_weighted)

bible_odd %>%
  group_by(part) %>% 
  slice_max(log_odds_weighted, n = 20) %>%
  ggplot(aes(x = log_odds_weighted,
             y = reorder(word, log_odds_weighted),
             fill = part)) +
  geom_col(show.legend = F) +
  facet_wrap(~part, scale = "free")
