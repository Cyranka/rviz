remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")


##
us_track <- x %>% group_by(year,state) %>%
  summarise(state_median = median(price_index),
            us_median = median(us_avg)) %>%
  select(year, us_median) %>% unique()

###


###
top_label <- x %>% group_by(year,state) %>%
  summarise(state_median = median(price_index),
            us_median = median(us_avg)) %>%
  ungroup() %>% filter(year == max(year)) %>%
  top_n(5, state_median)


down_label <- x %>% group_by(year,state) %>%
  summarise(state_median = median(price_index),
            us_median = median(us_avg)) %>%
  ungroup() %>% filter(year == max(year)) %>%
  top_n(-5, state_median)


##
x %>% group_by(year,state) %>%
  summarise(state_median = median(price_index),
            us_median = median(us_avg)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = state_median, group = state)) + 
  geom_line(size = 2, alpha = 0.2) + 
  geom_line(data = us_track,aes(x = year, y = us_median),
            inherit.aes = FALSE, size = 3, color = "firebrick2", alpha = 0.9) + 
  hrbrthemes::theme_modern_rc(axis_title_size = 13) + 
  theme(
    panel.grid.minor = element_blank()
  ) + 
  scale_x_continuous(breaks = c(seq(1975, 2015, by = 5), 2018)) + 
  labs(x = "Year",
       y = "Median Price Index",
       title = "Changes in State Price Index",
       subtitle = "Data aggregated by year. Red line represents the US average\nState labels shown represent top and bottom 5",
       caption = "Tidy Tuesday 2019 - Week 5") + 
  ggrepel::geom_text_repel(data = subset(top_label,year == max(year)),
                           mapping = aes(x = year, y = state_median, label = state),
                           size =3, segment.color = "black",color = "white", fontface = "bold", family = "Roboto") + 
  ggrepel::geom_text_repel(data = subset(down_label,year == max(year)),
                           mapping = aes(x = year, y = state_median, label = state),
                           size =3,segment.size = 0,segment.color = "black",color = "white", fontface = "bold", family = "Roboto") 
  
  


