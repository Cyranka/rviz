remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(lubridate)

x <- readRDS("rstats_tweets.rds")


url_regex <- '(http|https)[^([:blank:]|\\"|<|&|#\n\r)]+'

x <- x %>% 
  mutate(created_at = ymd(str_extract(x$created_at,
                                      "[0-9]{4}-[0-9]{2}-[0-9]{2}")),
         text = rtweet::plain_tweets(str_replace(text, url_regex, ""))) %>%
  filter(year(created_at) >= 2009)


#
tidy_tuesday <- x %>% filter(str_detect(str_to_lower(text), "tidytuesday|tuesday"))

write_rds(tidy_tuesday, "tidy_tuesday_tweets.RDS")