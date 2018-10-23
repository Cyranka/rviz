remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_30/")
library(tidyverse);library(lubridate);library(ggridges)


##Need to remove duplicates
x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv") %>%
  mutate(release_date = mdy(release_date)) %>%
  mutate(decade = year(floor_date(release_date, years(10)))) %>%
  filter(!worldwide_gross ==0)

x <- x %>% mutate(log_gross_over_budget = log(worldwide_gross/production_budget,2))

x %>% 
  filter(!decade <1970) %>%
  ggplot(aes(x = log_gross_over_budget,
             y = factor(genre,
                        levels = c("Horror", "Drama", "Comedy", "Adventure","Action")),
             fill = genre,
             alpha = 0.5)) +
  labs(x = "\nWorldwide gross over budget (base 2 log)",
       y = "Genre",
       title = "Distribution of worldwide gross over budget by genre and decade",
       subtitle = "X-axis corresponds to the metric's base 2 log\nMovies before 1970 were removed",
       caption = "Tidy tuesday week 30: Horror movies and profit\n Median of each distribution in black") + 
  geom_density_ridges(panel_scaling = FALSE,
                      quantile_lines = TRUE,
                      quantiles = 2) + 
  facet_wrap(~decade,
             ncol = 1) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13, face = "bold")
    
  ) + 
  viridis::scale_fill_viridis(discrete = TRUE, option = "B", begin = 0.5) + 
  scale_x_continuous(limits = c(-15.25, 9),
                     breaks = c(-15, -10, -5, 0, 5,10,15)) + 
  geom_text(data = x %>% group_by(genre, decade) %>% filter(!decade < 1970) %>%
              summarise(log_gross_over_budget = median(log_gross_over_budget)),
            aes(x = log_gross_over_budget, y =  factor(genre,
                                                       levels = c("Horror", "Drama", "Comedy", "Adventure","Action")),
                label = round(log_gross_over_budget,1)),inherit.aes = FALSE,
            nudge_y = 0.5, nudge_x = 0.5, col = "black")

