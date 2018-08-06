remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidytuesday_week_19/")
library(tidyverse)
x <- read_csv("tidy_tuesday_week_19.csv")


##Incident Reduction
event_decrease <- x %>% filter(type_of_event == "incidents") %>%
  select(airline, year_range,n_events) %>%
  spread(year_range, n_events) %>%
  mutate(improvement = `00_14` - `85_99`) %>%
  arrange(desc(improvement),`00_14`) %>%
  mutate(order = 1:56)

##
x %>% filter(type_of_event == "incidents") %>%
  mutate(year_range = relevel(factor(year_range, labels = c("00-14", "85-99")),ref = "85-99")) %>%
  inner_join(event_decrease %>% select(airline, improvement, order)) %>%
  ggplot(aes(x = reorder(airline, order), y = n_events, color = year_range, group = airline)) + geom_point(size = 2,alpha = 0.5) + coord_flip() + 
  geom_line(color = "black") + theme_minimal() + labs(color = "Period", y = "Total Events", x = "Airlines",
                                                      title = "Total Events by Airline",
                                                      subtitle = "Data sorted by airlines with the highest decrease in events",
                                                      caption = "Airlines on top: decrease in # of events\nAirlines in the middle: No change in # of events\nAirlines in the bottom: Increase in the # of events") + 
  scale_color_manual(values = c("firebrick", "steelblue")) + 
  geom_vline(xintercept = 26.5, size = 0.3, linetype = 2) +
  geom_vline(xintercept = 16.5, size = 0.3, linetype = 2)

##
library(factoextra)
library(cluster)
event_spread <- x %>% filter(type_of_event == "incidents") %>%
  select(airline, year_range,n_events) %>%
  spread(year_range, n_events) %>%
  select(airline, `85_99`,`00_14`)


for_cluster <- event_spread %>% select(-airline)

set.seed(5)
i <- kmeans(for_cluster %>% filter(`00_14`<12 & `85_99` <20), 4, nstart = 25)
k_means_centers <- as_tibble(round(i$centers,2)) %>% mutate(cluster = 1:4)


event_spread %>%filter(`00_14`<12 & `85_99` <20) %>%
  mutate(cluster = i$cluster) %>%
  ggplot(aes(x = `85_99`, y = `00_14`, color = as.character(cluster))) + geom_point(size = 4, shape=18) + geom_jitter(size = 4, shape = 18) + 
  theme_bw() + labs(color = 'K-Means Assignment') + geom_point(data = k_means_centers, aes(x=`85_99`, y= `00_14`), col = "black", size = 3,
                                                               shape = 4) + 
  labs(x = "Total events between 1985-1999", y = "Total events between 2000-2014",
       title = "K-means clustering of airline events",
       caption = "Cluster centers marked with an X\nSome 'extreme' cases were removed")
