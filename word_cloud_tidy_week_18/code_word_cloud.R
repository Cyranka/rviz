remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/word_cloud_tidy_week_18/")
source("/Users/harrocyranka/Desktop/code/twitter_info_analysis_3.R")
library(rtweet);library(tidytext)



##Unnest Tokens
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"


x <- search_tweets("tidytuesday", n = 5000,include_rts = FALSE)
x <- x %>% mutate(text = plain_tweets(text))


x_1 <- x %>% select(screen_name,text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex",pattern = unnest_reg) %>% filter(!word %in% stop_words$word,
                                                                             str_detect(word,"[a-z]"))

total_words <- x_1 %>%count(word, sort = TRUE)
colors <- ifelse(total_words$n >1,"blue", "firebrick")

total_words %>% 
  wordcloud2::wordcloud2(fontFamily = "roboto", color = colors, shuffle = FALSE)


##get bigrams
tidy_bigrams <- x %>% select(screen_name, text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "ngrams",n = 2)

count_bigrams <- tidy_bigrams %>% group_by(word) %>% tally() %>% arrange(desc(n)) %>% ungroup() %>% separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(word, c("word1", "word2"), sep = " ")

colors_bigrams <- ifelse(total_words$n >1,"blue", "firebrick")

count_bigrams %>% 
  wordcloud2::wordcloud2(fontFamily = "roboto", color = colors_bigrams, shuffle = FALSE,
                        size = 0.5)
