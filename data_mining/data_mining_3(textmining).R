#library installed
library(janeaustenr)
library(dplyr)
library(stringr)

#generate data for text mining
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()

#tokenization
library(tidytext)
original_books %>%
  unnest_tokens(word, text) -> tidy.text

#anti join with stop words 
data("stop_words")

tidy.text <- tidy.text %>%
  anti_join(stop_words)

tidy.text %>%
  count(word, sort = TRUE)

#visualization
library(ggplot2)
tidy.text %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#text mining with gutenbergr
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159), mirror = "http://mirrors.xmission.com/gutenberg/")

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767), mirror = "http://mirrors.xmission.com/gutenberg/")

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

#comparison three author's book
library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy.text, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

#visualization
library(scales)

## expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

#pearson's correlation
cor.test(data = frequency[frequency$author ==
                            "Bronte Sisters",],
         ~ proportion + `Jane Austen`) #cor == 0.737
cor.test(data = frequency[frequency$author ==
                            "H.G. Wells",],
         ~ proportion + `Jane Austen`) #cor  == 0.414


