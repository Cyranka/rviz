remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse);library(statebins)
state_milk <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv") %>%
    mutate(milk_produced = milk_produced/1000000)  

x <- state_milk %>%
    select(-region) %>%
    inner_join(fbinfo::pop_geo %>% select(state ,region) %>%
                   mutate(region = ifelse(region == "Western Overseas", "West", region)))

##Don't forget to aggregate
x %>% drop_na() %>%
    group_by(region,year) %>%summarise(total_milk = sum(milk_produced)) %>%
    ggplot(aes(x = year, y = total_milk,fill = region)) +
    geom_area(color = "gray90", alpha = 0.7) + 
    scale_fill_viridis_d(option = "D") + 
    hrbrthemes::theme_ipsum_rc() + 
    labs(x = "Year", y = "Total milk produced (millions of pounds)",
         fill = "Region",
         title = "Production of milk by region between 1970 and 2017",
         subtitle = "There has been a noticeable growth in the share of the West") + 
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
    ) + 
    scale_x_continuous(breaks = c(seq(1970,2010, by = 10),2017)) + 
    scale_y_continuous(labels = scales::comma) + 
    guides(fill = guide_legend(label.position = "bottom", keywidth = 3,keyheight = 0.5))