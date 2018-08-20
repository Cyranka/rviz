remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext);library(tm);library(lubridate)
csv_files <- grep("\\.csv",list.files(), value = TRUE)


x <- bind_rows(lapply(csv_files, function(i) read_csv(i)))
my_words <- readxl::read_excel("most_common_words_english_for_twitter.xlsx")

set.seed(4)
sample_1 <- x %>% filter(language == "English" & is.na(post_type)) %>%
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
with_sentiment <- sample_2 %>% dplyr::select(account_type, content, day) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  anti_join(my_words) %>% inner_join(get_sentiments(lexicon = "afinn")) %>%
  filter(word != "trump")

##
aggregate_sentiment <- with_sentiment %>% filter(day > ymd("2015-01-01")) %>%
  mutate(account_type = str_to_title(account_type)) %>%
  group_by(account_type, day) %>% dplyr::summarise(daily_score = sum(score)) %>%
  filter(account_type %in% c("Right", "Left"))

##first  
aggregate_sentiment %>% 
  filter(day > ymd("2016-01-01")) %>%
  mutate(month = floor_date(day, unit = "month")) %>%
  group_by(month, account_type) %>%
  summarise(monthly_median = median(daily_score)) %>%
  ggplot(aes(x = month, y = monthly_median, fill = monthly_median >0)) +
  geom_col(show.legend = FALSE, color = "black") + facet_wrap(~account_type) + theme_bw() + 
  scale_fill_manual(values = c("firebrick","blue")) + 
  labs(y = "Daily Score", x = "Day", title = "Median Daily Sentiment by Month",
       subtitle = "Data for Political Bots")


##Find Worst scoring months
aggregate_sentiment %>% 
    filter(day > ymd("2016-01-01")) %>%
    mutate(month = floor_date(day, unit = "month")) %>%
    group_by(month, account_type) %>%
    summarise(monthly_median = median(daily_score)) %>%
    arrange(monthly_median, account_type) ##Worst months for the right are
                                           ##August, September, October 2017

##Doing a TF-IDF of those months,
###     comparing to three before, and three after

target_period <- c(ymd("2017-08-01"),ymd("2017-09-01"),ymd("2017-10-01"))
previous <- c(ymd("2017-05-01"),ymd("2017-06-01"),ymd("2017-07-01"))
after <- c(ymd("2017-11-01"),ymd("2017-12-01"),ymd("2018-01-01"))

for_tfidf <- sample_2 %>% filter(day > ymd("2017-05-01") & day < ymd("2017-11-01")) %>%
    filter(account_type == "Right") %>%
    mutate(period = paste0(month(month),"-",year(month)))


##Start TF-IDF
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

##Get Right Wing First
tidy_right <- for_tfidf %>% dplyr::rename(text = content) %>%
    filter(account_type == "Right") %>%
    dplyr::select(period,text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text) 

total_words <- tidy_right %>% 
    dplyr::group_by(period) %>% 
    dplyr::summarize(total = n())

list_tf_right <- tidy_right %>% group_by(period, word) %>% summarise(total_period = n()) %>%
    left_join(total_words) %>%
    bind_tf_idf(word, period, total_period) %>% anti_join(stop_words)


list_tf_right %>%
    dplyr::select(-total) %>%
    arrange(desc(tf_idf))


list_tf_right %>% arrange(desc(tf_idf)) %>%
    mutate(nchar = nchar(word)) %>% filter(nchar >3) %>%
    dplyr::select(-nchar) %>%
    group_by(period) %>% arrange(period, desc(tf_idf)) %>% 
    slice(1:20) %>%
    ungroup() %>% group_by(word) %>%
    top_n(1,tf_idf) %>%
    ggplot(aes(reorder(word,tf_idf), tf_idf, fill = period)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~period,scales = "free") +
    coord_flip() + theme_bw()