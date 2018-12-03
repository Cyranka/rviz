remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(tidytext);library(tm)

##From wide to long and filter out empty titles
x <- read_csv("medium_datasci.csv") %>% select(-x1)%>%
  gather(tag, value, -(title:author_url)) %>%
  filter(value == 1) %>%
  filter(!is.na(title)) %>%
  mutate(tag = str_to_title(str_replace_all(str_replace_all(tag,"tag\\_", ""), "_"," ")))


tokens_unnested <- x %>% select(title) %>% 
  mutate(title = removePunctuation(title)) %>%
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word, title)


tokens_filtered <- tokens_unnested %>% group_by(word) %>% filter(n() >=20) %>% 
  anti_join(stop_words) %>%
  ungroup()

