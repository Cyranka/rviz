remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("")
library(rtweet)
library(tm)

timeline <- get_timeline("humansofny",n = 2500)

x <- timeline %>% filter(is_retweet == FALSE & is.na(reply_to_screen_name)) %>%
  mutate(text = plain_tweets(text))

saveRDS(x, "/Users/harrocyranka/Desktop/rviz/humans_of_ny_sentiment/tweets_retrieved.RDS")




