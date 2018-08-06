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
  mutate(year_range = factor(year_range, labels = c("00-14", "85-99"))) %>%
  inner_join(event_decrease %>% select(airline, improvement, order)) %>%
  ggplot(aes(x = reorder(airline, order), y = n_events, color = year_range, group = airline)) + geom_point(size = 2,alpha = 0.5) + coord_flip() + 
  geom_line(color = "black") + theme_minimal() + labs(color = "Year Range", y = "Total Events", x = "Airlines",
                                                      title = "Total Events by Airline",
                                                      subtitle = "Data sorted by airlines with the highest decrease in events",
                                                      caption = "Dotted line separates airlines into three groups") + 
  scale_color_manual(values = c("firebrick", "steelblue")) + 
  geom_vline(xintercept = 26.5, size = 0.3, linetype = 2) +
  geom_vline(xintercept = 16.5, size = 0.3, linetype = 2)