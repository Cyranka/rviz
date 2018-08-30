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

week_totals  %>%
  ggplot(aes(x = game_year, y = game_week, fill = percent_pass*100, label = round(percent_pass,3)*100)) + geom_tile(color = "black") + 
  theme_minimal() + 
  scale_fill_gradient2(low = "steelblue", high = "red",mid = "white",
                       midpoint = 67,
                       labels = c("60","64","67","70","74"),
                       breaks = c(60.5,64,67,70,74.0)) + 
  scale_x_continuous(breaks = seq(2000, 2017, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 16, by = 1)) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black", size = 0.1),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        plot.title = element_text(size = 15, face = "bold")) + 
  labs(x = "Year", y = "Week", fill = "% of passing yards",
       title = "% of passing yards as a proportion of total yards",
       subtitle = "There has been an increase in the number of yards obtained through passing",
       caption = "Tidy Tuesday Week 22") + geom_text(color = "black", size =3)


