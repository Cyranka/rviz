remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(lubridate)

x <- read_csv("nfl_data_tidy_data.csv")

##Looking at pass_yds, rec_yds

week_totals <- x %>% group_by(game_year, game_week) %>%
    summarise(total_rush = sum(rush_yds, na.rm = TRUE),
              total_pass = sum(pass_yds, na.rm = TRUE)
    ) %>% 
    mutate(total_yards = total_rush + total_pass) %>%
    mutate(percent_rush = total_rush/total_yards,
           percent_pass = total_pass/total_yards) %>%
    arrange(percent_pass)

##Create graph
week_totals %>%
    ggplot(aes(x = game_year, y = percent_pass*100,
               color = as.character(game_week),group = game_week)) +
    geom_line(show.legend = FALSE, size = 1.5) + facet_wrap(~game_week, scale = "free_x") + 
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, size = 0.3, color = "black",
                linetype = 2) + 
    theme_bw() + labs(x = "Year", y = "% of Passing Yards",
                           title = "% of passing yards as proportion of total yards: by week",
                           subtitle = "Linear trend in black",
                      caption = "Tidy Tuesday Week 22")