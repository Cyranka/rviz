remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harro.cyranka/Desktop/rviz/march_12/")
library(tidyverse);library(RColorBrewer)

##Add decade
x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv") %>%
  mutate(description = rtweet::plain_tweets(description)) %>% mutate(decade  = year_published - year_published%%10) %>%
  mutate(decade = factor(decade))

fit_function <- lm(average_rating~decade, data = x)

pred_df <- tibble(
  year_published = x$year_published,
  decade = x$decade,
  predicted_score = fit_function$fitted.values
)


x %>%
  ggplot(aes(x = year_published, y = average_rating, color = decade)) + 
  geom_jitter(show.legend = TRUE, alpha = 0.6, height =0.15, size = 0.5) +
  geom_line(aes(x = year_published, y = predicted_score),
            inherit.aes = FALSE, data = pred_df,linetype = 1, size =1,color = "cyan") + 
  scale_y_continuous(limits = c(1,10),
                     breaks = seq(1,10,by = 1)) +
  scale_x_continuous(breaks = c(seq(1950,2010, by = 10))) + 
  labs(x = "\nYear published",
       y = "Average rating\n",
       title = "Recent games have been, on average, rated higher",
       subtitle = "Games published in the 2010s were rated, on average, 1.1 points higher than games released in the 1950s\n",
       caption = "Tidy Tuesday 2019: Board Games Database\nWhite line represents a step function fit") + 
  theme_minimal() + 
  scale_color_brewer(palette = "Spectral", direction = -1) + 
  theme(
    text = element_text(family = "Roboto", color = "gray90"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "gray30"),
    plot.title = element_text(size = 20,face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(family = "Roboto", color = "gray90"),
    axis.title = element_text(family = "Roboto", color = "gray90", face = "bold"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_line(size =0.1),
    panel.grid.major.x = element_line(size =0.1),
    panel.grid.minor.x = element_blank()
  ) + 
  guides(
    color = guide_legend(
      title = "Decade",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      label.position = "bottom",
      override.aes =list(alpha = 1,size = 4))
  )
   