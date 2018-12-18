remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_38/")
library(tidyverse);library(viridis)

x <- read_csv("all_cetacean_data.csv") %>%
  mutate(originLocation = case_when(
    originLocation %in% c("Unknown, US","Unknown, FL, US") ~ "Unknown",
    TRUE ~originLocation
  ),
  currently = case_when(
    currently == "Dolphin Quest Hawai'i" ~ "Dolphin Quest Hawaii",
    currently == "Unknown, FL, US" ~ "Unknown",
    TRUE~currently
  ))


##
or_dest <- group_by(x,originLocation, currently) %>% tally(sort = TRUE) %>%
  ungroup() %>% mutate(cum_prop = cumsum(n)/sum(n)) %>%
  filter(n > 10) 

##
library(ggraph)
library(igraph)


object <- graph_from_data_frame(or_dest)
set.seed(30)
ggraph(object, layout = 'linear', circular = TRUE) + 
  geom_edge_arc2(aes(edge_width = n, edge_alpha = n), 
                 color = "cyan") + 
  geom_node_point(size = 3,
                  color = "gray80") + 
  geom_node_text(aes(label = name),
                 repel = TRUE, size = 2.5,
                 color = "white",
                 fontface = "bold",
                 segment.alpha = 0) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 18, color = "White",face = "bold"),
    plot.subtitle = element_text(size = 13, color = "White",face = "bold"),
    plot.caption = element_text(size = 10, color = "White", face = "bold"),
    legend.text = element_text(color = "white", face = "bold"),
    legend.title = element_text(color = "white", face = "bold")
  ) + 
  scale_edge_width(range = c(0.5,15), name = "Total transfers", guide = "legend",
                   breaks = c(10,20,30,40,50,75,100,125),
                   labels = as.character(c(10,20,30,40,50,75,100,125))) + 
  scale_edge_alpha(guide = "none", range = c(0.3,0.9)) + 
  guides(edge_width = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) + 
  labs(title = "Network of origin and destination of cetaceans",
       subtitle = "Pairs that appeared less than 10 times were removed", 
       caption = "Tidy tuesday week 38\nCetacean data")
  