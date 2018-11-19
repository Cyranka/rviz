remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/7_train/")
library(tidyverse);library(tidytext)
library(igraph)
library(ggraph)
library(widyr)

x <- readRDS("train_7_tweets.RDS") %>%
  mutate(text = rtweet::plain_tweets(text))

#Delays
delays <- x %>% filter(str_detect(text,"#Delays"))

tidy_delays <- delays %>% select(status_id, text) %>% 
  mutate(text = tm::removePunctuation(tm::removeNumbers(text))) %>%
  unnest_tokens(word,text,token = "tweets") %>% anti_join(stop_words) %>%
  filter(nchar(word) > 2)


##
word_correlations <- tidy_delays %>% group_by(word) %>% filter(n() >=10) %>% anti_join(data_frame(word = c("sthudson","stelmhurst",
                                                                                                           "stbound","metswillets","flushingmain",
                                                                                                           "blvdjackson","blvdjackson","stbroadway"))) %>%
  pairwise_cor(word, status_id, sort = TRUE, upper = FALSE) %>% filter(!is.infinite(correlation))


