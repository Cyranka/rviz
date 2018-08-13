remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext)

x <-read_csv("sampled_tweets.csv") %>%
  filter(language == "English")


x <- x %>% mutate(content = rtweet::plain_tweets(content))

##Begin data processing
right_left <- x %>% filter(account_category %in% c("RightTroll", "LeftTroll")) %>%
  mutate(publish_date = lubridate::mdy(gsub("\\s.*","",publish_date))) %>%
  mutate(harvested_date = lubridate::mdy(gsub("\\s.*","",harvested_date))) %>%
  mutate(year = lubridate::year(publish_date))


##Right wing accounts
right <- right_left %>% filter(account_category == "RightTroll" & year >=2015)
left <- right_left %>% filter(account_category == "LeftTroll" & year >=2015)

##
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"


##Get Right Wing First
tidy_right <- right %>% dplyr::rename(text = content) %>%
  select(year,text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex",pattern = unnest_reg) 

total_words <- tidy_right %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(total = n())

list_tf_right <- tidy_right %>% group_by(year, word) %>% summarise(total_year = n()) %>%
  left_join(total_words) %>%
  bind_tf_idf(word, year, total_year) %>% anti_join(stop_words)


list_tf_right %>%
  select(-total) %>%
  arrange(desc(tf_idf))

list_tf_right %>% arrange(desc(tf_idf)) %>%
  mutate(nchar = nchar(word)) %>% filter(nchar >3) %>%
  dplyr::select(-nchar) %>%
  group_by(year) %>% arrange(year, desc(tf_idf)) %>% 
  slice(1:20) %>%
  ungroup() %>%
  mutate(word = ifelse(year == 2015, paste0(word," "),
                       ifelse(year == 2016, paste0(" ",word),
                              ifelse(year == 2017, paste0("  ",word),word)))) %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = as.character(year))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year,scales = "free") +
  coord_flip() + theme_bw() +
  labs(x = "Word", y = "TF-IDF", title = "TF-IDF of Right Wing Troll: Data for Selected Years", 
       subtitle = "Based on 68,263 Tweets from a random sample of 300,000 tweets",
       caption = "Tidy Tuesday week 20, Russian Bot Data")


##Repeat for Left Wing
tidy_left <- left %>% dplyr::rename(text = content) %>%
  select(year,text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex",pattern = unnest_reg) 

total_words <- tidy_left %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(total = n())

list_tf_left <- tidy_left %>% group_by(year, word) %>% summarise(total_year = n()) %>%
  left_join(total_words) %>%
  bind_tf_idf(word, year, total_year) %>% anti_join(stop_words)


list_tf_left %>%
  select(-total) %>%
  arrange(desc(tf_idf))

list_tf_left %>% arrange(desc(tf_idf)) %>%
  mutate(nchar = nchar(word)) %>% filter(nchar >3) %>%
  dplyr::select(-nchar) %>%
  group_by(year) %>% arrange(year, desc(tf_idf)) %>% 
  slice(1:20) %>%
  ungroup() %>%
  mutate(word = ifelse(year == 2015, paste0(word," "),
                       ifelse(year == 2016, paste0(" ",word),
                              ifelse(year == 2017, paste0("  ",word),word)))) %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = as.character(year))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year,scales = "free") +
  coord_flip() + theme_bw() +
  labs(x = "Word", y = "TF-IDF", title = "TF-IDF of Left Wing Trolls: Data for Selected Years", 
       subtitle = "Based on 44,858 Tweets from a random sample of 300,000 tweets",
       caption = "Tidy Tuesday week 20, Russian Bot Data")
