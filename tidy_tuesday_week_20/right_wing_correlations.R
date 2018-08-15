remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext)
csv_files <- grep("\\.csv",list.files(), value = TRUE)

set.seed(4)
x <- bind_rows(lapply(csv_files, function(i) read_csv(i)))

set.seed(5)
right_sample <- x %>% filter(account_type %in% c("right","Right") & language == "English") %>%
  sample_n(20000) %>% mutate(content = rtweet::plain_tweets(content))

##Convert into Tidy Text
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets <- right_sample %>% mutate(text = content) %>%
  select(author, text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex",pattern = unnest_reg) %>% filter(!word %in% stop_words$word,
                                                                             str_detect(word,"[a-z]"))
##Get Correlations
library(tidyr)
library(gtools)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)

word_cors <- tidy_tweets%>% group_by(word) %>% filter(n() >= 30) %>% pairwise_cor(word, author, sort = TRUE)
word_cors <- word_cors %>% mutate(to_filter = as.numeric(row.names(word_cors)) %%2) %>% filter(to_filter == 0) %>% mutate(to_filter = NULL)


set.seed(5)
word_cors %>% ungroup() %>%
  filter(item1 !="debalwaystrump") %>%
  filter(item2 !="debalwaystrump") %>%
  filter(correlation> .85) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_label(aes(label = name), repel = TRUE, size = 3) +
  theme_minimal() + labs(title ="Word correlations within tweets published by right wing bots", y = "", x= "",
                         subtitle = "Correlations over 0.85")

##
word_cors %>% filter(item1 %in% c("trump","army", "cnn","congress")) %>%
  group_by(item1) %>% filter(item2!="trump's") %>% 
  filter(item2 !="debalwaystrump") %>%
  top_n(10) %>% ungroup() %>%
  filter(item2!="video") %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) + geom_bar(stat = "identity", show.legend = FALSE) + facet_wrap(~item1, scales = "free") + coord_flip() + 
  labs(y = "Correlation", x = "Word", title = "Top correlations for selected words",
       subtitle = "Data for right wing bots") + theme_bw()

##Get related words
army_words <- word_cors %>% filter(item1 %in% c("army")) %>%
  filter(item2!="return") %>%
  group_by(item1) %>% top_n(10) %>%
  pull(item2) %>% append("army")

congress_words <- word_cors %>% filter(item1 %in% c("congress")) %>%
  filter(item2 !="debalwaystrump") %>%
  group_by(item1) %>% top_n(10) %>%
  pull(item2) %>% append("congress")

trump_words <- word_cors %>% filter(item1 %in% c("trump")) %>%
  filter(item2!="trump's") %>%
  group_by(item1) %>% top_n(10) %>%
  pull(item2) %>% append("trump")

#
##search for term function
search_term <- function(word, data_frame){
  x <- data_frame
  k <- x[grep(word, x$content),] %>%
    dplyr::select(content, publish_date)
  print(paste0("Retrieved ",word))
  return(k)
}

##
library(lubridate)
list_1 <- lapply(1:length(army_words), function(i)search_term(army_words[i], x %>% filter(account_type %in% c("right", "Right"))))
army_df <- bind_rows(list_1) %>% unique()

army_by_month <- army_df %>% mutate(publish_date = mdy_hm(publish_date)) %>%
  mutate(month_year = floor_date(publish_date,unit = "month")) %>%
  group_by(month_year) %>% summarise(total = n()) %>%
  arrange(month_year) %>% mutate(group = "Army Words")

##Searching for Congress
list_2 <- lapply(1:length(congress_words), function(i)search_term(congress_words[i], x %>% filter(account_type %in% c("right", "Right"))))
congress_df <- bind_rows(list_2) %>% unique()


congress_by_month <- congress_df %>% mutate(publish_date = mdy_hm(publish_date)) %>%
  mutate(month_year = floor_date(publish_date,unit = "month")) %>%
  group_by(month_year) %>% summarise(total = n()) %>%
  arrange(month_year) %>% mutate(group = "Congress Words")

##Searching for DJT words
list_3 <- lapply(1:length(trump_words), function(i)search_term(trump_words[i], x %>% filter(account_type %in% c("right", "Right"))))
djt_df <- bind_rows(list_3) %>% unique()


djt_by_month <- djt_df %>% mutate(publish_date = mdy_hm(publish_date)) %>%
  mutate(month_year = floor_date(publish_date,unit = "month")) %>%
  group_by(month_year) %>% summarise(total = n()) %>%
  arrange(month_year) %>% mutate(group = "Trump Words")


##
bind_rows(army_by_month, congress_by_month, djt_by_month) %>%
  filter(month_year > ymd("2014-01-01")) %>%
  ggplot(aes(month_year, y = total, color = group)) + geom_line(size = 0.5, show.legend = FALSE) + theme_bw() +
  #geom_point(aes(x = get_date, y = value),color = "black", data = djt_dates) + 
  #geom_label(aes(x = get_date, y = value, label = my_label),color = "black",data = djt_dates, size = 3, alpha = 0.5) + 
  #geom_label(aes(x = get_date, y = value, label = my_label),color = "black",data = blm_dates, size = 3, alpha = 0.5) + 
  facet_wrap(~group) + 
  labs(x = "Time", y = "Total",
       title = "Total tweets containing groups of selected keywords",
       subtitle = "Data for right wing bots - Data aggregated monthly")
