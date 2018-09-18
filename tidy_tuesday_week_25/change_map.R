remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_25/")
library(tidyverse)

x <- read_csv("us_airports.csv") %>% 
  filter(!is.na(passengers))

my_labels <- c("-31.2% to -1.8%",
               "+0.5% to +7.8%",
               "+9.5% to +11.8%",
               "+12.2% to +18.1%",
               "+18.6% to +22.1%",
               "+26.8% to +35.5%")

passenger_variation <- x %>%
  group_by(state, year) %>%
  summarise(total_passengers = sum(passengers,na.rm = TRUE))%>%
  filter(year %in% c(2012, 2017)) %>% ungroup() %>%
  spread(year, total_passengers) %>%
  mutate(change = round((`2017`/`2012`-1)*100,1)) %>%
  mutate(bucket = cut_number(change, 6)) %>%
  filter(state %in% urbnmapr::states$state_abbv) %>%
  mutate(bucket = factor(bucket, labels = my_labels))  

##

##
passenger_variation %>%
  inner_join(urbnmapr::states, by = c("state" = "state_abbv")) %>%
  ggplot(aes(x = long, y = lat, fill = bucket, group = group)) + 
  geom_polygon(size = .45,color = "gray25") +
  theme_minimal()+ 
  scale_fill_brewer(palette = "Reds") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 11, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust =0.5)) +
  guides(fill = guide_legend(title = "% Change", title.position = "top", 
                               title.hjust = 0.5, barwidth = 20,
                               barheight = 0.5,keywidth = 4,keyheight = 1,nrow = 2)) + 
  labs(title = "% Change in total passengers between 2012 and 2017",
       subtitle = "Highest increases happened in Washington, Idaho, and Oregon",
       caption = "Tidy Tuesday Week 25\nAirport data")

 

