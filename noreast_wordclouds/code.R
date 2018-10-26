rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/noreast_wordclouds/")
library(tidyverse);library(tidytext);library(wordcloud2)

x <- read_csv("noreast_api_users.csv")

tidy <- x %>% rename(text = description) %>%
    mutate(text = rtweet::plain_tweets(text)) %>%
    select(text) %>%
    filter(!is.na(text)) %>%
    unnest_tokens(word, text, token = "tweets")

tidy %>% anti_join(stop_words) %>%
    group_by(word) %>% tally(sort = TRUE) %>%
    filter(nchar(word) >2) %>% slice(1:200) %>%
    wordcloud2(fontFamily = "Arial", size = 1, color = "steelblue")
    