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
    arrange(percent_pass) %>%
  mutate(week_label = paste0("Week ", as.character(game_week))) %>%
  mutate(week_label = factor(week_label, levels = paste0("Week ", 1:16)))

##Create graph
week_totals %>%
    ggplot(aes(x = game_year, y = percent_pass*100,
               color = as.character(game_week),group = game_week)) +
    geom_line(show.legend = FALSE, size = 1.5) + facet_wrap(~week_label, scale = "free_x") + 
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, size = 0.4, color = "black",
                linetype = 2) + 
    theme_bw() + labs(x = "Year", y = "% of Passing Yards",
                           title = "% of passing yards as proportion of total yards: yearly changes displayed by season week",
                           subtitle = "Linear trend in black",
                      caption = "Tidy Tuesday Week 22") + 
  theme(plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        plot.title = element_text(size = 13, face = "bold"),
        strip.background = element_rect(fill ="dimgray"),
        strip.text = element_text(color="white", size = 10, face = "bold"))

##Get Model totals
by_week <- week_totals %>% group_by(game_week) %>% 
  select(game_week, game_year, percent_pass) %>% arrange(game_week,game_year) %>%
  mutate(percent_pass = percent_pass*100) %>%
  nest()

fit_model <- function(df){
  lm(percent_pass ~ game_year, data = df)
}

##Fit Models
year_models <- map(by_week$data, fit_model)

##generate distributions
set.seed(4)
simulations <- lapply(1:16, function(i)arm::sim(year_models[[i]],2000))

##Retrieve coefficients
coefficient_distribution <- bind_rows(lapply(1:16, function(i)simulations[[i]]@coef[,2] %>% as_tibble() %>%
                                     mutate(week = i)))

median_coef <- coefficient_distribution %>% group_by(week) %>%
  summarise(median_coef = median(value))

coefficient_distribution %>%
  ggplot(aes(x = week, y = value, group = week)) + geom_boxplot(outlier.size = 0.3) + 
  geom_point(aes(x = week, y = median_coef), data = median_coef, color = "red") +
  scale_x_continuous(breaks = c(1:16)) +
  labs(x = "Week", y = "Coefficient value",
       title = "Simulated distribution of linear trend coefficients",
       subtitle = "Higher coefficient values for weeks 1, 8, and 9",
       caption = "Tidy Tuesday Week 22\n2,000 simulations of linear trend coefficients") + 
  theme_bw() + 
  theme(plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        plot.title = element_text(size = 15, face = "bold"),
        panel.grid.minor = element_blank())
