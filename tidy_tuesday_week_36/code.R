remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(tidytext);library(tm)

##From wide to long and filter out empty titles
x <- read_csv("medium_datasci.csv") %>% select(-x1)%>%
  gather(tag, value, -(title:author_url)) %>%
  filter(value == 1) %>%
  filter(!is.na(title)) %>%
  mutate(tag = str_to_title(str_replace_all(str_replace_all(tag,"tag\\_", ""), "_"," ")))


tokens_unnested <- x %>% select(title) %>% 
  mutate(title = removeNumbers(removePunctuation(title))) %>%
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word, title)


tokens_filtered <- tokens_unnested %>% group_by(word) %>% filter(n() >=20) %>% 
  anti_join(stop_words) %>% anti_join(readxl::read_excel("most_common_words_english_for_twitter.xlsx")) %>%
  ungroup()

##
tdm <- tokens_filtered %>% count(doc_id, word) %>%
  cast_tdm(word, doc_id, n)


##
tdm2 <- removeSparseTerms(tdm, sparse = 0.993)

hc <- hclust(dist(tdm2), method = "average")
plot(hc, yaxt = "n", main = "@Tidytuesday week 36 dendrogram", hang = -1)

###
library(widyr)
library(igraph)
library(ggraph)
word_pairs <- tokens_filtered %>% pairwise_count(word, doc_id, sort = TRUE)

word_pairs <- word_pairs %>%
  mutate(to_filter = as.numeric(row.names(word_pairs)) %%2) %>% filter(to_filter == 0) %>%
  mutate(to_filter =NULL) ##Remove obvious duplicates


#
set.seed(15)
word_pairs %>%
  filter(n >= 200) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = n,edge_width = n),
                 show.legend = TRUE,
                 check_overlap = TRUE,
                 color = "gray75") +
  geom_node_point(size = 5,
                  color = "red") +
  geom_node_text(aes(label = name),
                 repel = TRUE, size = 3.5,
                 color = "white",
                 fontface = "bold",
                segment.alpha = 0) +
  theme_void() + 
  theme(
    text = element_text(family = "Roboto"),
    panel.grid = element_line(color = "white", size = 0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 18, color = "White",face = "bold"),
    plot.subtitle = element_text(size = 13, color = "White",face = "bold"),
    plot.caption = element_text(size = 10, color = "White"),
    legend.text = element_text(color = "white", face = "bold"),
    legend.title = element_text(color = "white", face = "bold")
   )  + 
  scale_edge_width(range = c(0.5,5), name = "Pairwise counts", guide = "legend",
                   breaks = c(200,400,600,800,1000,2000,4000,6000),
                   labels = c("200", "400", "600", "800", "1,000", "2,000", "4,000", "6,000")) +
  scale_edge_alpha(range = c(0.6,1), breaks = c(0.5,0.6,0.7,0.8,0.9,1)) + 
  guides(edge_width = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) + 
  labs(title = "Pairwise counts of words in the titles of Medium data science articles",
       subtitle = "Pairs that appeared less than 200 times were removed", 
       caption = "Tidy tuesday week 36\nMedium data science articles metadata")