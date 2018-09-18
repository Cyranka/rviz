remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_25/")
library(tidyverse);library(geofacet)

x <- read_csv("us_airports.csv") %>% 
  filter(!is.na(passengers)) %>%
  mutate(hub_type = ifelse(is.na(hub_type), "Nonhub", hub_type)) %>%
  mutate(state = ifelse(airport_name %in% c("Washington Dulles International",
                                            "Ronald Reagan Washington National"),"DC",state))

totals_by_year <- x %>%
  group_by(state, year) %>%
  summarise(total_passengers = sum(passengers,na.rm = TRUE))%>%
  ungroup() %>%
  spread(year, total_passengers)


state_names <- read_csv("states_abbreviations.csv")
  
##
totals_by_year %>%
  gather(year, total, -state) %>%
  mutate(total  = round(total/1000000,2),
         year = as.numeric(year) - 2000) %>%
  inner_join(state_names, by = c("state" = "abbreviation")) %>%
  select(state.y, year, total) %>% rename(state = state.y) %>%
  ggplot(aes(year, total, group = 1)) + 
  geom_smooth(se = FALSE, col = "black", size = 1) + 
  geom_smooth(se = FALSE, method = "lm", col = "red", linetype = 2, size = 0.5) + 
  facet_geo(~state, grid = "us_state_grid2",scale = "free") + 
  theme(axis.text.y = element_text(size = 6),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 6),
        strip.text.x = element_text(size = 6.5),
        plot.title = element_text(size = 13,face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) + 
  labs(x = "Year", y = "Total passengers (in millions)",
       title = "Yearly changes in total number of passengers by state",
       subtitle = "Reagan National and Dulles International assigned to DC\nLoess fit in black; OLS fit in red",
       caption = "Tidy Tuesday Week 25\nAirport data")
  