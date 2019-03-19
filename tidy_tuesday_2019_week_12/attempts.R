remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse);library(socviz)

x <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv") %>%
  mutate(location = str_to_title(location)) %>%
  rename(county = location)

total_stops <-x %>% group_by(state, county) %>%
  summarise(total_stops = sum(stops_per_year, na.rm = TRUE))

plot_data <-  x %>%
  select(county, state, driver_race, stop_rate) %>%
  spread(driver_race, stop_rate) %>%
  filter(!is.na(White),
         Black >0) %>%  
  mutate(region = case_when(
    state %in% c("FL", "NC", "TX", "SC") ~ "South",
    state %in% c("CT","MA", "RI", "NJ", "VT") ~ "Northeast",
    state %in% c("MT", "OH","WI", "IL") ~ "Midwest",
    TRUE ~ "West"
  )) %>%inner_join(total_stops)



label_data <- plot_data  %>%
  mutate(b_w = Black/White) %>% arrange(desc(b_w)) %>%
  slice(1:10) %>%
  bind_rows(plot_data %>%
              mutate(b_w = Black/White) %>% arrange(b_w) %>% slice(1:10))


plot_data %>%
  ggplot(aes(x = White, y = Black,color = log(Black/White))) + 
  geom_point(show.legend = FALSE) +
    geom_abline(slope = 1) + 
  scale_x_log10() + 
  scale_y_log10() +
  scale_size_continuous(range = c(0.5,20)) + 
  scale_color_gradient2(low= "blue", high = "chocolate1", mid = "gray80", midpoint = 1) + 
  theme_minimal() + 
  theme(
    
  ) + facet_wrap(~region)