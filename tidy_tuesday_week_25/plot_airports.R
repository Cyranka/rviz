remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse)

x <- read_csv("us_airports.csv")
y <- read_csv("states_abbreviations.csv")
z <- read_csv("airports_locations.csv")

##
matched_airports <- x %>% filter(year == 2016) %>%
  inner_join(y, by = c("state"= "abbreviation")) %>%
  select(-state) %>%
  rename(state = state.y) %>%
  select(state, loc_id, passengers, airport_classification) %>%
  inner_join(z, by = c("loc_id" = "iata")) %>%
  filter(country == "United States" & !state %in% c("Alaska", "Hawaii"))

urbnmapr::states %>% filter(!state_abbv %in% c("AK","HI")) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon() + 
  geom_polygon(size = .45,color = "gray35", fill = "black") +
  theme_minimal() +
  geom_point(aes(long, lat,size = passengers,
                 color = airport_classification), inherit.aes = FALSE, data = matched_airports %>%
               filter(long > -140), alpha = 0.65) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        legend.position = "bottom") + 
  labs(color = "Airport classification", size = "Total passengers",
       title = "Airport locations in the lower 48 states in 2016",
       subtitle = "Total airports matched: 701",
       caption = "Tidy Tuesday Week 25") + 
  guides(color = guide_legend(title.position = "top", 
                             title.hjust = 0.5, barwidth = 20,
                             barheight = 0.5,keywidth = 4,keyheight = 1,nrow = 3, ncol = 1),
         size = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 2)) + 
  scale_color_manual(values = c("red", "chocolate1", "yellow")) + 
  scale_size_continuous(range = c(2,15),
                        labels = c("0","1,000,000",
                                   "2,000,000",
                                   "3,000,000",
                                   "4,000,000",
                                   "5,000,000"))