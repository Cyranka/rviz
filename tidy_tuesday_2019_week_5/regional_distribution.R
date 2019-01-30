remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse);library(statebins)

x <- fbinfo::pop_geo %>%as_tibble() %>%
    select(abbreviation, region) %>%
    mutate(region = ifelse(region == "Western Overseas","West", region))
    

x %>% rename(value = region) %>%
    statebins(state_col = "abbreviation",
    value_col = "value",
    font_size = 3,
    dark_label = "black",
    light_label = "white",
    ggplot2_scale_function = scale_fill_manual,
    name = "Region",
    values = viridis::viridis(n = 5,option = "D",alpha = 0.8),
    round = TRUE
) + theme_statebins() + 
    theme(
        plot.title = element_text(face = "bold")
    ) + 
    labs(title = "Regional distribution used for Tidy Tuesday week 5") + 
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5,nrow = 1))

