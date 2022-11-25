#set environment to LDA
library(topicmodels)
data("AssociatedPress")

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

#Word-Topic Probability
library(tidytext)
ap_topic <- tidy(ap_lda, matrix = "beta")
ap_topic

#visualization
library(dplyr)
library(ggplot2)

ap_top_term <- ap_topic %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup %>%
  arrange(topic, -beta)

ap_top_term %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#terms with greatest difference in ????
library(tidyr)

beta_spread <- ap_topic %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))
