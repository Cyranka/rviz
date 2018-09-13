remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/carolinas_evacuation_map/")
library(tidyverse);library(urbnmapr)

x <- readxl::read_excel("counties_evacuated.xlsx") %>%
  mutate(to_paint = "Yes")


my_labels <- get_urbn_labels(map = "counties") %>% inner_join(x, by = c("county_name"= "County", "state_name" = "State")) %>%
  filter(state_name %in% c("South Carolina", "North Carolina"))

x %>%
  right_join(urbnmapr::counties,by = c("State" = "state_name", "County" = "county_name")) %>%
  filter(state_abbv %in% c("NC","SC")) %>%
  mutate(to_paint = ifelse(is.na(to_paint), "No",to_paint)) %>%
  ggplot(aes(long, lat, group = group, fill = to_paint)) + 
  geom_polygon(color = "gray75", size = .25) + theme_minimal() +
  scale_fill_manual(values = c("azure2", "dodgerblue4")) +  
  labs(title = "Map of counties currently evacuated in North and South Carolina",
       subtitle ="As of September 12, 2019",
       caption = "Source: https://weather.com/safety/hurricane/news/2018-09-11-florence-evacuation-orders-states") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 13, color = "black", hjust = 0.5),
        plot.caption = element_text(size = 9),
        #plot.background = element_rect(fill = "gray20"),
        legend.position = "bottom") +
  guides(fill = guide_legend(keywidth = 4, keyheight = 0.5, title="County Evacuated?", label.position = "bottom", label.hjust = 0.5,title.vjust = 1)) + 
  ggrepel::geom_label_repel(data = my_labels, aes(x = long, lat, label = county_name), 
            size = 2.2 , inherit.aes = FALSE, color = "black",segment.size = 0.1, segment.color = "white")