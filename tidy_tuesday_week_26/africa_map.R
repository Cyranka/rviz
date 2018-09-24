remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

# setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_26/")
library(tidyverse);library(viridis)
x <- read_csv("african_species.csv")

species_by_country <- x %>% group_by(country) %>% tally() %>%
  mutate(country = ifelse(country == "Gambia (the)", "Gambia",
                          ifelse(country == "United Republic of Tanzania", "Tanzania", country)))


africa_map <- map_data(map = "world") %>% filter(region %in% read_csv("african_countries.csv")$Country)
adding_map <- species_by_country %>% right_join(africa_map, by = c("country" = "region")) %>%
  mutate(n = ifelse(is.na(n), 0, n))



adding_map %>%
  ggplot(aes(x = long, y = lat, group = group, fill = log(n,base = 2))) +
  geom_polygon(col = "gray45", size = 0.5, alpha = 0.9) + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(angle = 90),
        legend.position = "bottom",
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 13)) + 
  scale_y_continuous(limits = c(-46.96289,47.34038)) + 
  scale_fill_viridis(option = "D",breaks = c(1,2,3,4,5,6,7,8,9,10,11),
                     labels = c(as.character(2**c(1,2,3,4,5,6,7,8,9,10,11))),
                     limits = c(0,11)) +
  guides(fill = guide_colorbar(title = "Number of invasive species by country",
                              title.position = "top",title.hjust = 0.5,barwidth = 15)) + 
  labs(x = "", y = "", title = "Gradient map of invasive species by African country",
       subtitle = "South Africa has the highest number of invasive species\nCountries in gray represent countries with no invasive species or missing",
       caption = "Tidy Tuesday week 26 - Invasive species in Africa")