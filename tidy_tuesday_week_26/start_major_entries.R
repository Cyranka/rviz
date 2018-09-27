remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_26/")
library(readxl);library(tidyverse)

air <- read_csv("total_airports_by_country.csv") %>% select(-date_info)
ports <- read_csv("ports_of_the_world.csv") %>%
  rename(major_ports = n) 


ports_air <- air %>% left_join(ports, by = c("country" = "country"))

##
species_by_country <- read_csv("african_species.csv") %>% group_by(country) %>% tally() %>%
  mutate(country = ifelse(country == "Gambia (the)", "Gambia",
                          ifelse(country == "United Republic of Tanzania", "Tanzania", country)))

##
with_entries <- species_by_country %>% left_join(ports_air) %>% 
  mutate(major_ports = ifelse(is.na(major_ports), 0, major_ports)) %>%
  mutate(major_entries = total_airports +major_ports) %>%
  left_join(read_excel("african_status.xlsx")) %>%
  mutate(status = ifelse(is.na(status), "Coastal",status))




top_10 <- species_by_country %>% left_join(ports_air) %>% 
  mutate(major_ports = ifelse(is.na(major_ports), 0, major_ports)) %>%
  mutate(major_entries = total_airports +major_ports) %>% arrange(desc(n)) %>%
  slice(1:8) %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country)) %>%
  bind_rows(species_by_country %>% left_join(ports_air) %>% 
              mutate(major_ports = ifelse(is.na(major_ports), 0, major_ports)) %>%
              mutate(major_entries = total_airports +major_ports) %>% arrange(desc(major_entries)) %>%
              slice(1:8) %>%
              mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country))) %>% unique()

with_entries %>% ggplot(aes(x = major_entries,y = n, label = country, color = status)) + 
  geom_point(size = 4.5,alpha =0.75) + 
  theme_minimal() + 
  labs(x = "\nTotal points of entry (airports and major ports)",
       y = "Number of invasive species",
       title = "Scatterplot of total invasive species by total points of entry",
       subtitle = "Unsurprisingly, there is a positive relationship between the two variables",
       caption = "Tidy tuesday week 26 - Invasive species in Africa\nAirports: CIA World Factbook\nPorts:exportvirginia.org") + 
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 18, face = "bold", color = "white"),
    plot.caption = element_text(size = 10, color = "white"),
    plot.subtitle = element_text(size = 12, color = "white"),
    axis.title.x = element_text(color = "white",face = "bold", size = 12),
    axis.text.x = element_text(color = "white", face= "bold", size = 12),
    axis.title.y = element_text(color = "white", face = "bold", size = 12),
    axis.text.y = element_text(color = "white", face = "bold", size = 12),
    panel.grid.major = element_line(size = 0.15, linetype = 2),
    panel.grid.minor = element_line(size = 0.15, linetype = 2),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold"),
    legend.text = element_text(color = "white"),
    text = element_text(family = "Courier")
  ) +
  scale_x_continuous(limits = c(0,610),
                     breaks = c(0,100,200,300,400,500,600))  + 
  ggrepel::geom_text_repel(aes(x = major_entries, y = n, label = country), data = top_10, color = 'white', size =4,direction = "both",
                           segment.alpha = 0,segment.size = 1,point.padding = 0.24) + 
  guides(color = guide_legend(title = "Country type", title.position = "top", title.hjust = 0.5)) + 
  scale_colour_manual(values = c("cyan", "firebrick1","lawngreen"))



##Remove South African and Seychelles
# with_entries %>% filter(!country %in% c("South Africa", "Seychelles")) %>%
#   ggplot(aes(x = major_entries,y = n, label = country, color = status)) + 
#   geom_point(size = 4.5,alpha =0.75) + 
#   theme_minimal() + 
#   labs(x = "\nTotal points of entry (airports and major ports)",
#        y = "Number of invasive species",
#        title = "Scatterplot of total invasive species by total points of entry",
#        subtitle = "Labeled points display top countries by total number of invasive species",
#        caption = "Tidy tuesday week 26 - Invasive species in Africa\nTotal airports: CIA World Factbook\nPorts:exportvirginia.org") + 
#   theme(
#     plot.background = element_rect(fill = "black"),
#     plot.title = element_text(size = 15, face = "bold", color = "white"),
#     plot.caption = element_text(size = 10, color = "white"),
#     plot.subtitle = element_text(size = 13, color = "white"),
#     axis.title.x = element_text(color = "white",face = "bold", size = 12),
#     axis.text.x = element_text(color = "white", face= "bold", size = 12),
#     axis.title.y = element_text(color = "white", face = "bold", size = 12),
#     axis.text.y = element_text(color = "white", face = "bold", size = 12),
#     panel.grid.major = element_line(size = 0.15, linetype = 2),
#     panel.grid.minor = element_line(size = 0.15, linetype = 2),
#     panel.grid.minor.x = element_blank(),
#     legend.position = "bottom",
#     legend.title = element_text(color = "white", face = "bold"),
#     legend.text = element_text(color = "white")
#   ) +
#   scale_x_continuous(limits = c(0,300),
#                      breaks = c(0,50,100,150,200, 250, 300)) + 
#   scale_y_continuous(limits = c(0,600)) + 
#   guides(color = guide_legend(title = "Nation status", title.position = "top", title.hjust = 0.5)) + 
#   scale_colour_manual(values = c("cyan", "firebrick1","lawngreen")) + 
#   geom_smooth(aes(x = major_entries, y = n),method = "lm", se = FALSE, inherit.aes = FALSE, 
#               color = "white",
#               data = with_entries %>%
#                 filter(!country %in% c("South Africa", "Seychelles")))