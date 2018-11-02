rmeove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/noreastr/")
library(tidyverse);library(tidytext);library(lubridate)

load("noreastr18.rda")

#
no_rt <- noreastr18 %>% filter(is_retweet == FALSE) %>%
  mutate(text = rtweet::plain_tweets(text))


#Basic dataset
x <- no_rt %>% select(screen_name, created_at, text) %>%
  mutate(created_at = floor_date(created_at, "day"))

#
tidy_tweets <- x %>% 
  mutate(text = tm::removeNumbers(text)) %>%
  unnest_tokens(word, text, token = "tweets") %>% anti_join(stop_words) %>%
  group_by(word) %>% filter(nchar(word) > 1)

#
##Get Correlations
library(tidyr)
library(gtools)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)


word_cors <- tidy_tweets%>% group_by(word) %>% filter(n() >= 5) %>% pairwise_cor(word, screen_name, sort = TRUE)
word_cors <- word_cors %>% mutate(to_filter = as.numeric(row.names(word_cors)) %%2) %>% filter(to_filter == 0) %>% mutate(to_filter = NULL)


##Correlation graph
set.seed(200)
word_cors %>%
  filter(correlation> .6) %>%
  rename(Correlation = correlation) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = Correlation,edge_width = Correlation), show.legend = TRUE,color = "gray75") +
  geom_node_point(color = "red", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, color = "white", fontface = "bold",
                  segment.alpha = 0) +
  theme_minimal() + labs(title ="Word correlations for tweets with the hashtag #noreastr18", y = "", x= "",
                         subtitle = "Only correlations over 0.6 are displayed",
                         caption = "#noreastr18 tweets") + 
  theme(
    text = element_text(family = "Roboto"),
    panel.grid = element_line(color = "white", size = 0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 20, color = "White",face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 17, color = "White",face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 12, color = "White",face = "bold"),
    legend.text = element_text(color = "white", face = "bold"),
    legend.title = element_text(color = "white", face = "bold")
  ) + 
  scale_edge_width(range = c(1,2))

##S
