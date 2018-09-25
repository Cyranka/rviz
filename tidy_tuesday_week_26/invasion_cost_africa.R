remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

# setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_26/")
library(tidyverse);library(viridis)
x <- read_csv("african_species.csv")

cost <- read_csv("table_3.csv")

species_by_country <- x %>% group_by(country) %>% tally() %>%
  ungroup() %>%
  mutate(country = ifelse(country == "Gambia (the)", "Gambia",
                          ifelse(country == "United Republic of Tanzania", "Tanzania", country)))

africa_map <- map_data(map = "world") %>% filter(region %in% read_csv("african_countries.csv")$Country)
 
with_cost <- species_by_country %>% left_join(cost) %>% mutate(invasion_cost = invasion_cost/1000000) %>%
  select(country,n,invasion_cost, gdp_proportion)


adding_map <- with_cost %>% right_join(africa_map, by = c("country" = "region")) %>%
  mutate(n = ifelse(is.na(n), 0, n))

  
adding_map %>%
  ggplot(aes(x = long, y = lat, group = group, fill = gdp_proportion)) +
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
  scale_y_continuous(limits = c(-46.96289,47.34038))