remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_2019_week_18/")
library(tidyverse);library(widyr);library(tidytext);
library(igraph);library(ggraph);library(grid)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

x <- nobel_winner_all_pubs %>%
  filter(affiliation == "university of chicago") %>%
  select(doi,title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  filter(str_detect(word, "[0-9]",negate = TRUE)) %>%
  filter(nchar(word)>3) %>%
  group_by(word) %>% filter(n()>5) %>%ungroup() %>%
  pairwise_count(word,doi)

##Place image
logo <- magick::image_read("images.png")
raster <- as.raster(logo)

##graph
set.seed(400)
x %>%
  filter(n > quantile(n, 0.96)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = n,edge_width = n),
                 show.legend = TRUE,
                 check_overlap = TRUE,
                 color = "gray40") + 
  geom_node_point(size = 5,
                  color = "#801717") +
  geom_node_text(aes(label = name),
                 repel = TRUE, size = 3,
                 color = "black",
                 fontface = "bold",
                 segment.alpha = 0) +
  theme_void() + 
  theme(
    text = element_text(family = "Impact"),
    panel.grid = element_line(color = "black", size = 0.02),
    plot.title = element_text(size = 15,family = "Impact",color = "black",face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11,family = "Impact",color = "black",face = "bold",hjust = 0.5),
    plot.background = element_rect(fill = "gray80"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    plot.caption = element_text(size = 18)
  ) + 
  scale_edge_width(range = c(0.5,5),name = "Pairwise counts", guide = "legend") + 
  scale_edge_alpha(breaks = c(1000,2000,3000)) + 
  guides(edge_width = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) + 
    labs(title = "Pairwise counts of words in titles of Nobel winning publications by researchers from the University of Chicago",
         subtitle = "Publications in physics and chemistry\n\n")

grid.raster(raster,width = 0.1,height = 0.1,just = c(-4,4.2))