remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext)
csv_files <- grep("\\.csv",list.files(), value = TRUE)

set.seed(4)
x <- bind_rows(lapply(csv_files, function(i) read_csv(i)))

set.seed(5)
right_sample <- x %>% filter(account_type == "left" & language == "English") %>%
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
word_cors %>%
  filter(correlation> .75) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "firebrick", size = 4) +
  geom_node_label(aes(label = name), repel = TRUE, size = 3) +
  theme_minimal() + labs(title ="Word correlations within tweets published by left wing bots", y = "", x= "",
                         subtitle = "Correlations over 0.75")

##Need to fix the graph
word_cors %>% filter(item1 %in% c("#nowplaying","#blacklivesmatter", "white","trump")) %>%
  group_by(item1) %>% filter(item2!="trump's") %>% top_n(10) %>% ungroup() %>%
  filter(item2!="video") %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) + geom_bar(stat = "identity", show.legend = FALSE) + facet_wrap(~item1, scales = "free") + coord_flip() + 
  labs(y = "Correlation", x = "Word", title = "Top correlations for selected words",
       subtitle = "Data for left wing bots") + theme_bw()


##Get Hip Hop Words
hip_hop_words <- word_cors %>% filter(item1 %in% c("#nowplaying")) %>%
  group_by(item1) %>% top_n(15) %>%
  pull(item2) %>% append("#nowplaying")

blm_words <- word_cors %>% filter(item1 %in% c("#blacklivesmatter")) %>%
  group_by(item1) %>% top_n(15) %>%
  pull(item2) %>% append("#blacklivesmatter")

trump_words <- word_cors %>% filter(item1 %in% c("trump")) %>%
  filter(item2!="trump's") %>%
  group_by(item1) %>% top_n(15) %>%
  pull(item2) %>% append("trump")


##search for term function
search_term <- function(word, data_frame){
  x <- data_frame
  k <- x[grep(word, x$content),] %>%
    dplyr::select(content, publish_date)
  print(paste0("Retrieved ",word))
  return(k)
}

##Not searching for 'playing' because it is too broad of a term
library(lubridate)
list_1 <- lapply(2:length(hip_hop_words), function(i)search_term(hip_hop_words[i], x %>% filter(account_type == "left")))
hh_df <- bind_rows(list_1) %>% unique()

hh_by_month <- hh_df %>% mutate(publish_date = mdy_hm(publish_date)) %>%
  mutate(month_year = floor_date(publish_date,unit = "month")) %>%
  group_by(month_year) %>% summarise(total = n()) %>%
  arrange(month_year) %>% mutate(group = "Music Words")


##Searching for BLM
list_2 <- lapply(1:length(blm_words), function(i)search_term(blm_words[i], x %>% filter(account_type == "left")))
blm_df <- bind_rows(list_2) %>% unique()


blm_by_month <- blm_df %>% mutate(publish_date = mdy_hm(publish_date)) %>%
  mutate(month_year = floor_date(publish_date,unit = "month")) %>%
  group_by(month_year) %>% summarise(total = n()) %>%
  arrange(month_year) %>% mutate(group = "BLM Words")

##Searching for DJT words
list_3 <- lapply(1:length(trump_words), function(i)search_term(trump_words[i], x %>% filter(account_type == "left")))
djt_df <- bind_rows(list_3) %>% unique()


djt_by_month <- djt_df %>% mutate(publish_date = mdy_hm(publish_date)) %>%
  mutate(month_year = floor_date(publish_date,unit = "month")) %>%
  group_by(month_year) %>% summarise(total = n()) %>%
  arrange(month_year) %>% mutate(group = "Trump Words")


##
djt_dates <- tibble(get_date = as.POSIXct(c("2016-11-08 UTC","2017-01-01 UTC","2015-06-01 UTC")),
                    value = c(2633, 2156,1765),
                    my_label = c("Election", "Inauguration", "Announcement"),
                    group = "Trump Words")

blm_dates <- tibble(get_date = as.POSIXct(c("2016-02-01 UTC","2017-04-01 UTC","2015-08-01 UTC")),
                    value = c(2114, 1367,697),
                    my_label = c("HRC Interrupted", "Jordan Edwards", "Sanders Interrupted"),
                    group = "BLM Words")

##
bind_rows(blm_by_month, hh_by_month, djt_by_month) %>%
  filter(month_year > ymd("2014-01-01")) %>%
  ggplot(aes(month_year, y = total, color = group)) + geom_line(size = 0.5, show.legend = FALSE) + theme_bw() +
  geom_point(aes(x = get_date, y = value),color = "black", data = djt_dates) + 
  geom_label(aes(x = get_date, y = value, label = my_label),color = "black",data = djt_dates, size = 3, alpha = 0.5) + 
  geom_label(aes(x = get_date, y = value, label = my_label),color = "black",data = blm_dates, size = 3, alpha = 0.5) + 
  facet_wrap(~group) + 
  labs(x = "Time", y = "Total",
       title = "Total tweets containing groups of selected keywords",
       subtitle = "Data for left wing bots - Data aggregated monthly")

##
