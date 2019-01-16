library(tidytext)
library(stringr)
library(tidyr)
library(gutenbergr)


# https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html

book1 <- gutenberg_download(1300)
christmas <- read.csv("christmascarol.csv", header = T, stringsAsFactors = F) %>% select(-X)
jungle <- read.csv("thejungle.csv", header = T, stringsAsFactors = F) %>% select(-X)
pinocchio <- read.csv("pinocchio.csv", header = T, stringsAsFactors = F) %>% select(-X)

full_data <- rbind(christmas, jungle, pinocchio, book1)


by_chapter <- full_data %>%
  group_by(gutenberg_id) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0)

by_chapter_word <- by_chapter %>%
  unite(title_chapter, gutenberg_id, chapter) %>%
  unnest_tokens(word, text)

word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(title_chapter, word, sort = TRUE)

chapters_dtm <- word_counts %>%
  cast_dtm(title_chapter, word, n)

chapters_dtm


library(topicmodels)
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

chapters_lda_td <- tidy(chapters_lda)
chapters_lda_td

top_terms <- chapters_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms


library(ggplot2)
theme_set(theme_bw())

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

