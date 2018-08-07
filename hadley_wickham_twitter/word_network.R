remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
library(tidytext)
library(widyr)

#Word Pairs Network
library(igraph)
library(ggraph)

word_network <- function(my_number){
  
  set.seed(123)
  bio_word_pairs <- readRDS("bio_word_pairs.RDS")
  the_number <- as.numeric(as.character(my_number))
  
  define_color <- "#018270"
  png(filename="word_network_twitter.png",width=591, height=422)  
  plot(bio_word_pairs %>%
         filter(n >= the_number) %>%
         graph_from_data_frame() %>%
         ggraph(layout = "fr") +
         geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = define_color) +
         geom_node_point(size = 3) +
         geom_node_text(aes(label = name), repel = TRUE, 
                        point.padding = unit(0.2, "lines")) +
         theme_void())
  dev.off()
}

define_number <- 200

word_network(define_number)
