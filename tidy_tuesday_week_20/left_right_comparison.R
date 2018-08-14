remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext)


csv_files <- grep("\\.csv",list.files(), value = TRUE)
x <- bind_rows(lapply(csv_files, function(i) read_csv(i))) 

english <- x %>% filter(language == "English")
##
#hashtags <- tibble(hashtag = unlist(stringr::str_extract_all(rtweet::plain_tweets(english$content),"#\\S+")))
#saveRDS(hashtags, "all_hashtags.RDS")

##political hashtags
politics <- x %>% filter(language == "English") %>%
  filter(account_category %in% c("RightTroll","LeftTroll"))

#pol_hashtags <- tibble(hashtag = unlist(stringr::str_extract_all(rtweet::plain_tweets(politics$content),"#\\S+")))
#saveRDS(pol_hashtags, "political_hashtags.RDS")
pol_hashtags <- readRDS("political_hashtags.RDS")

total_pols <- pol_hashtags %>% mutate(hashtag = str_to_lower(hashtag)) %>%
  count(hashtag, sort = TRUE) %>% filter(n > 100)

##politics, no rtweets or replies
politics <- politics %>% filter(is.na(post_type)) %>%
  mutate(content = rtweet::plain_tweets(content))

politics <- politics %>% mutate(content = gsub("'$|^'","",content)) %>%
  mutate(content = gsub('"$|^"',"",content)) %>%
  mutate(publish_date = lubridate::mdy(gsub("\\s.*","",publish_date)))


##
my_words <- readxl::read_excel("most_common_words_english_for_twitter.xlsx")
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_politics <- politics %>% select(content, publish_date, account_type) %>%
  mutate(content = tm::removeNumbers(str_replace_all(content, replace_reg, ""))) %>%
  mutate(content = gsub("[^[:alnum:][:space:]]","",content)) %>%
  unnest_tokens(word, content, token = "regex", pattern = unnest_reg) %>%
  anti_join(stop_words) %>% anti_join(my_words)

##
for_graph <- tidy_politics %>% mutate(account_type = ifelse(account_type == "right", "Right", account_type)) %>%
  group_by(account_type, word) %>% tally() %>%
  filter(n > 100) %>%
  left_join(tidy_politics %>%
              mutate(account_type = ifelse(account_type == "right", "Right", account_type)) %>%
              count(account_type, sort = TRUE) %>% rename(total = n)) %>%
  mutate(proportion = n/total)

library(scales)
for_graph %>% ungroup() %>%
  select(-n, -total) %>%
  spread(account_type, proportion) %>%
  ggplot(aes(left, Right)) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

