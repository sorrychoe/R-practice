library(httr)
library(tidyverse)
library(rvest)
library(tidytext)
library(wordcloud2)

news_url <- "https://news.jtbc.co.kr/section/index.aspx?scode=10"

news <- GET(news_url)

html <- read_html(news)

news_head <- html_nodes(html, 'a')

news_headline <- html_text(news_head)
news_headline

news_words <- as.data.frame(news_headline)
news_words_set <- news_words[98:165,]

news_words_set <- gsub('\n|\t','',news_words_set)
news_words_set <- gsub('play',NA,news_words_set)
news_words_set <- na.omit(news_words_set)

news_words_set <- tibble(news_words_set)
colnames(news_words_set) <- 'text'
news_words_set %>%
  unnest_tokens(word, text) -> word_set

word_set %>%
  count(word) %>%
  arrange(desc(n)) -> count_word

wordcloud2(data=count_word,fontFamily = 'ë§‘ì?€ê³ ë”•')


