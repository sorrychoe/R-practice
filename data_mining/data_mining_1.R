library(dplyr)

#chapter 1: data manipulation with dplyr
load(url('https://github.com/hbchoi/SampleData/raw/master/hflights.RData'))

x <- 1:10
x %>% sum() # it is same to sum(x)
            # %>% is pipe operator included to dplyr library

head(hf_data)
unique(hf_data$UniqueCarrier)

#연습 문제 1번 코드
UC <- hf_data %>%
  group_by(UniqueCarrier) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))
UC

#연습 문제 2번 코드
dest <- hf_data %>%
  group_by(Dest) %>%
  summarise(avg = mean(ActualElapsedTime, na.rm = T)) %>%
  arrange(avg)
head(dest)
tail(dest)

#연습 문제 3번 코드
cancelled <- hf_data %>%
  group_by(UniqueCarrier) %>%
  mutate(UC.prob = n()) %>%
  filter(Cancelled != 0) %>%
  mutate(Cancelled.prob = n()) %>%
  summarise(percent = Cancelled.prob/UC.prob) %>%
  arrange(desc(percent))
head(cancelled)
tail(cancelled)

#연습 문제 3번 보완 풀이 --> 더 정확하고 직관적인 결과 도출
CP <- hf_data %>%
  filter(Cancelled != 0) %>%
  group_by(UniqueCarrier) %>%
  summarise(Cancelled.prob = n()) %>%
  arrange(desc(Cancelled.prob))
UC.CP <- left_join(UC, CP, by = "UniqueCarrier") %>%
  group_by(UniqueCarrier) %>%
  summarise(percent = Cancelled.prob/freq) %>%
  arrange(desc(percent))
UC.CP

#chapter 2: data integration with dplyr
load(url('https://github.com/hbchoi/SampleData/raw/master/join_practice.RData'))

#연습문제 1번
left_join(bands, artists, by = c('first', 'last'))
right_join(bands, artists, by = c('first', 'last'))

#연습문제 2번
music <- full_join(songs, albums)
session <- full_join(artists, bands)
full_join(session, music)

#exercise 1
albums %>%
  semi_join(bands, by = "band") %>%
  nrow()

#exercise 2
songs %>%
  left_join(labels, by = 'album') %>%
  filter(!is.na(label)) %>%
  nrow()

#exercise 3
movie_years %>%
  left_join(movie_directors, by = c("movie" = "name"))%>%
  select(year, movie, artist = name, director, studio)
