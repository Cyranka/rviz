remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/7_train/")
library(tidyverse);library(tidytext)

x <- readRDS("train_7_tweets.RDS") %>%
  mutate(text = rtweet::plain_tweets(text))

#Delays
delays <- x %>% filter(str_detect(text,"#Delays"))

tidy_delays <- delays %>% select(status_id, text) %>% 
  mutate(text = tm::removePunctuation(tm::removeNumbers(text))) %>%
  unnest_tokens(word,text,token = "tweets") %>% anti_join(stop_words) %>%
  filter(nchar(word) > 2)

tdm_delays <- tidy_delays %>% group_by(status_id, word) %>% tally() %>%
  cast_tdm(word, status_id, n)

##Associations: Times square and Grand central
t_square <- tm::findAssocs(tdm_delays, "times",0.05)$times %>% broom::tidy()

g_central <- tm::findAssocs(tdm_delays, "grand",0.05)$grand %>% broom::tidy() %>% filter(x <0.5)

##
tdm2 <- tm::removeSparseTerms(tdm_delays, sparse = 0.98)


hc <- hclust(dist(tdm2), method = "complete")
plot(hc, yaxt = "n", main = "7 train delays",hang = -1, cex = 0.8)
