remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse)
state_milk <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv") %>%
  mutate(milk_produced = milk_produced/1000000) %>%
    select(-region) %>%
    inner_join(fbinfo::pop_geo %>% select(state ,region) %>%
                   mutate(region = ifelse(region == "Western Overseas", "West", region)))




state_milk %>%
  filter(year %in%c(1970,1980,1990,2001,2010,2017)) %>%
  ggplot(aes(x = milk_produced, y = fct_rev(factor(year)), group = year, fill = region)) + 
  ggridges::geom_density_ridges_gradient(show.legend = FALSE, 
                                         color = "white", scale = 1.5) + 
  labs(y = "Year", x = "\nTotal milk produced (millions of pounds) (log scale)", 
       title = "Changes in the distribution of milk production in the United States by region",
       subtitle = "Regional division differs from original dataset",
       caption = "Tidy tuesday week 5: dairy in the United States") +  
  scale_x_log10(labels = scales::comma) + 
    scale_y_discrete(expand = c(0,2)) + 
  scale_fill_viridis_d(option = "D",alpha = 0.9) + 
  hrbrthemes::theme_modern_rc() + 
  theme(
    text = element_text(size = 14, color = "white", family = "Helvetica"),
    axis.title  = element_text(color = "white", size = "14", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", size =0.1),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    strip.background = element_rect(fill ="black", color = "black"),
    strip.text = element_text(color = "white", face = "bold")
  ) + 
  facet_wrap(~fct_rev(fct_reorder(region,x = milk_produced, .fun = sum)), ncol = 1, scales = "free_y")