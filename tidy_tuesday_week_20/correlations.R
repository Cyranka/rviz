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


word_cors %>%
  filter(correlation> .75) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

##Need to fix the graph
word_cors %>% filter(item1 %in% c("black","#blacklivesmatter", "#policebrutality","white")) %>%
  group_by(item1) %>% top_n(10) %>% ungroup() %>%
  filter(item2!="video") %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) + geom_bar(stat = "identity", show.legend = FALSE) + facet_wrap(~item1, scales = "free") + coord_flip() + 
  labs(y = "Correlation", x = "Word") + theme_minimal()