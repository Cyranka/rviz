remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_33/")
library(tidyverse);library(viridis);library(gridExtra)

x <- read_csv("malaria_deaths_age.csv")

y <- read_csv("region_csv.csv") %>%
  select(`alpha-3`, region) %>%
  magrittr::set_colnames(c("code", "region"))

x <- x %>% inner_join(y)

##Africa graph
africa_graph <- x %>% group_by(age_group, year, region) %>%
  summarise(median_death = median(deaths)) %>% ungroup() %>%
  mutate(age_group = factor(age_group, levels = c("Under 5", "5-14","15-49","50-69", "70 or older"))) %>%
  arrange(year, age_group) %>% filter(region == "Africa") %>%
  ggplot(aes(x = year, y = median_death, color = age_group)) + geom_line(size = 1.5) + 
  geom_point(size = 3, show.legend = FALSE) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 11,angle = 90),
    axis.text.y = element_text(size = 11),
    axis.line.x = element_line(size = 0.5,color = "black"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.background = element_rect(fill = "gray95"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11)
  ) + 
  scale_x_continuous(breaks = seq(1990,2016,by = 1)) + 
  scale_y_continuous(breaks = seq(0,3500,by = 500),
                     labels = prettyNum(seq(0,3500,by = 500),big.mark = ",")) + 
  scale_color_viridis(option = "B",
                     discrete = TRUE,
                     begin = 0.3,
                     end = 0.8) + 
  labs(
    x = "\nYear",
    y = "Median number of deaths",
    title = "Malaria deaths by age group in Africa",
    subtitle = "Deaths in the under 5 age group outpace all others",
    caption = "Tidy tuesday week 33: Malaria dataset"
  ) + 
  guides(color = guide_legend(
    title = "Age group",
    title.position = "top",
    title.hjust = 0.5,
    label.position = "bottom",
    label.hjust = 0.5))


##Asia graph
asia_graph <- x %>% group_by(age_group, year, region) %>%
  summarise(median_death = median(deaths)) %>% ungroup() %>%
  mutate(age_group = factor(age_group, levels = c("Under 5", "5-14","15-49","50-69", "70 or older"))) %>%
  arrange(year, age_group) %>% filter(region == "Asia") %>%
  ggplot(aes(x = year, y = median_death, color = age_group)) + geom_line(size = 1.5) + 
  geom_point(size = 3, show.legend = FALSE) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 11,angle = 90),
    axis.text.y = element_text(size = 11),
    axis.line.x = element_line(size = 0.5,color = "black"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.background = element_rect(fill = "gray95"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11)
  ) + 
  scale_x_continuous(breaks = seq(1990,2016,by = 1)) + 
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(0,4,1)) + 
  scale_color_viridis(option = "B",
                      discrete = TRUE,
                      begin = 0.3,
                      end = 0.8) + 
  labs(
    x = "\nYear",
    y = "Median number of deaths",
    title = "Malaria deaths by age group in Asia",
    subtitle = "There has been an overall downward trend across all age groups",
    caption = "Tidy tuesday week 33: Malaria dataset"
  ) + 
  guides(color = guide_legend(
    title = "Age group",
    title.position = "top",
    title.hjust = 0.5,
    label.position = "bottom",
    label.hjust = 0.5))

##
gridExtra::grid.arrange(africa_graph, asia_graph, ncol = 1)


##Linear trends
x %>% 
  filter(region == "Africa") %>%
  group_by(entity,year) %>%
  summarise(median_death = median(deaths)) %>%
  ggplot(aes(x = year, y = median_death, group = entity)) + 
  geom_line(size = 3, alpha = 0.2) + 
  theme_minimal()