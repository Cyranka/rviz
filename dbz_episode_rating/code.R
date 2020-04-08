remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/dbz_episode_rating/")
library(tidyverse)

x <- readxl::read_excel("work_book_episodes.xlsx")  %>%
    janitor::clean_names() %>%
    mutate(wiki_season = factor(wiki_season),
           wiki_season = fct_rev(fct_reorder(wiki_season,season))) %>%
    group_by(wiki_season) %>%
    mutate(
        rolling_episode = row_number()
    ) %>% ungroup()

x %>% select(wiki_season, rolling_episode, average_rating) %>%
    spread(rolling_episode, average_rating) %>%
    gather(rolling_episode, average_rating, -wiki_season) %>%
    mutate(rolling_episode = parse_number(rolling_episode)) %>%
    ggplot(aes(x = rolling_episode,
               y = wiki_season,
               fill = average_rating,
               label = average_rating)) + 
    geom_tile(color = "gray20") + 
    geom_text(family = "Roboto Condensed", size = 3, fontface = "bold") + 
    labs(x = "\nEpisode", 
         y = "Season\n",
         title = "Dragon Ball Z Episode Ratings",
         subtitle = "The episode 'Final Atonement' (episode 18 of the Babidi Saga) had the highest average rating\nThis is the episode where Vegeta sacrifices himself to save Trunks",
         caption = "Data obtained at: https://www.ratingraph.com/tv-shows/dragon-ball-z-ratings-9875/") + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 12) + 
    scale_fill_gradient2(low = "red3",
                         mid = "yellow2",
                         midpoint = 7.26,
                         high = "green4",na.value = "gray40",
                         breaks = c(5.5,6.5,7.5,8.5)) + 
    theme(
        text = element_text(color = "white"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "gray20"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2,color = "black", linetype = 2)
        
    ) + 
    scale_x_continuous(breaks = c(1:38)) + 
    guides(fill = guide_colorbar(title = "Average rating",
                                 title.position = "top",
                                 title.hjust = 0.5,
                                 barwidth = 15,
                                 barheight = 0.5))


