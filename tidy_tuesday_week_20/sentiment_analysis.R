remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext);library(tm);library(lubridate)
csv_files <- grep("\\.csv",list.files(), value = TRUE)


x <- bind_rows(lapply(csv_files, function(i) read_csv(i)))
my_words <- readxl::read_excel("most_common_words_english_for_twitter.xlsx")

set.seed(4)
sample_1 <- x %>% filter(language == "English") %>%
  sample_n(60000) %>% mutate(content = rtweet::plain_tweets(content))


##Define clean variable function
clean_text_variable <- function(the_vector){
  library(tm)
  the_vector <- stringi::stri_trans_general(the_vector, "latin-ascii")
  #change to lower case, only alpha-numeric
  the_vector <- stringr::str_trim(gsub("[^[:alnum:] ]", " " , tolower(the_vector)))
  #kills websites
  the_vector <- stringr::str_trim(gsub('http.* *', '', the_vector))
  #removes punctuation
  the_vector <- removePunctuation(the_vector)
  #removes numbers
  the_vector <- removeNumbers(the_vector)
  #remove whitespace
  the_vector <- stripWhitespace(the_vector)
  ##
  #remove whitespace
  the_vector <- stringr::str_trim(stripWhitespace(the_vector))
}

sample_2 <- sample_1 %>% mutate(content = clean_text_variable(content)) %>%
  mutate(publish_date = floor_date(mdy_hm(publish_date),unit = "day")) %>%
  mutate(month = floor_date(publish_date, "month")) %>%
  mutate(day = floor_date(publish_date, "day")) %>%
  mutate(account_type = ifelse(account_type == "right", "Right", account_type))

##
with_sentiment <- sample_2 %>% select(account_type, content, day) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  anti_join(my_words) %>% inner_join(get_sentiments(lexicon = "afinn")) %>%
  filter(word != "trump")

##
aggregate_sentiment <- with_sentiment %>% filter(day > ymd("2015-01-01")) %>%
  mutate(account_type = str_to_title(account_type)) %>%
  group_by(account_type, day) %>% dplyr::summarise(daily_score = sum(score)) %>%
  filter(account_type %in% c("Right", "Left"))

##Do 2016 first  
aggregate_sentiment %>% filter(day >= ymd("2016-01-01") & day <= ymd("2016-12-31")) %>%
  filter(account_type == "Right") %>%
  ggplot(aes(x = day, y = daily_score, fill = daily_score >0)) +
  geom_col(show.legend = FALSE) + facet_wrap(~account_type) + theme_bw() + 
  scale_fill_manual(values = c("firebrick","blue"))

##Getting streaks 2016
streaks_2016 <- aggregate_sentiment %>% filter(day >= ymd("2016-01-01") & day <= ymd("2016-12-31")) %>%
  mutate(daily_sentiment = ifelse(daily_score >= 0, "Positive","Negative")) %>%
  group_by(account_type, daily_sentiment) %>% filter(account_type == "Right") %>%
  ungroup() %>%
  mutate(streak = c(1,rep(0,345)))

my_vector <- vector(mode = "numeric", length = 345)
my_vector[1] <- 1
for(i in 2:346){
  my_vector[i] <- ifelse(streaks_2016$daily_sentiment[i] == streaks_2016$daily_sentiment[i-1],my_vector[i-1]+1,1)  
  print(i)
}
remove(i)

streaks_2016$streak <- my_vector

##Finding top streaks
streaks_2016 %>% top_n(3,streak) %>% arrange(streak)


##Wordclouds for selected days
streaks_2016 %>% top_n(3,streak) %>% arrange(streak) %>% 
  mutate(begin_date = ymd(day) - 11) %>%
  select(account_type, begin_date, day)

sample_2 %>% filter(publish_date >=ymd("2016-01-29") & publish_date <= ymd("2016-02-09") & account_type == "Right") %>%
  unnest_tokens(word, content) %>% anti_join(stop_words) %>% anti_join(my_words) %>%
  group_by(word) %>% tally(sort = TRUE) %>% slice(1:40) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip()


with_sentiment  %>% filter(day >=ymd("2016-01-29") & day <= ymd("2016-02-09") & account_type == "Right") %>% View()

sample_2 %>% filter(publish_date >=ymd("2016-09-09") & publish_date <= ymd("2016-09-20") & account_type == "Right") %>%
  unnest_tokens(word, content) %>% anti_join(stop_words) %>% anti_join(my_words) %>%
  group_by(word) %>% tally(sort = TRUE) %>% slice(1:40) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip()

with_sentiment  %>% filter(day >=ymd("2016-09-09") & day <= ymd("2016-09-20") & account_type == "Right") %>% View()


sample_2 %>% filter(publish_date >=ymd("2016-12-09") & publish_date <= ymd("2016-12-20") & account_type == "Right") %>%
  unnest_tokens(word, content) %>% anti_join(stop_words) %>% anti_join(my_words) %>%
  group_by(word) %>% tally(sort = TRUE) %>% slice(1:40) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip()