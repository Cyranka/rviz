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
  labs(x = "Year", y = "Week", fill = "% of total yards",
       title = "% of passing yards as a proportion of total yards",
       subtitle = "There has been an increase in the number of yards obtained through passing",
       caption = "Tidy Tuesday Week 22") + geom_text(color = "black", size =3)


##Add some conference analysis
y <- read_csv("teams_by_conference.csv")

x2 <- x %>% inner_join(y)

confs <- x2 %>% group_by(conference,division, game_year) %>%
  summarise(total_rush = sum(rush_yds, na.rm = TRUE),
           total_pass = sum(pass_yds, na.rm = TRUE)
  ) %>% 
  mutate(total_yards = total_rush + total_pass) %>%
  mutate(percent_rush = total_rush/total_yards,
         percent_pass = total_pass/total_yards) %>%
  arrange(percent_pass) %>%
  arrange(conference,division,game_year) 


confs%>% 
  ggplot(aes(x = game_year, y = percent_pass*100, color = division)) + 
  geom_smooth(method = "lm",
             #formula = y~splines::bs(x,7),
             se = FALSE, size = 0.5) + 
  geom_point(shape = 18) + 
  facet_wrap(~conference,nrow=12) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(2000, 2017, by = 1)) + 
  #scale_y_continuous(breaks = seq(59, 72, by = 1)) + 
  labs(x = "Year", y = "% of total yards", color = "Division",
       title = "% of passing yards as a proportion of total yards",
       subtitle = "Different patterns emerge as we group the data by conference and division") + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gainsboro", size = 0.3))