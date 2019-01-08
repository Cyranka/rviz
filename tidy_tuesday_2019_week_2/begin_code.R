remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(viridis);library(lubridate);library(ggthemes)

x <- read_csv("IMDb_Economist_tv_ratings.csv")

mean_ratings <- x %>% group_by(year(date)) %>%
  summarise(avg_rate = mean(av_rating)) 

##
three_year <- x %>% mutate(three_year_time = year(x$date) - year(x$date) %% 3) %>%
  group_by(three_year_time, title) %>%
  summarise(avg_rating = mean(av_rating*share)) %>%
  ungroup() %>% group_by(three_year_time) %>% top_n(1,avg_rating) %>%
  ungroup() %>%
  select(three_year_time, title) %>% unique() %>%
  filter(title != "Band of Brothers")


##Fix df
fix_df <- x %>% filter(title %in% three_year$title) %>%
  mutate(title = case_when(
    title == "The X-Files" & year(date) > 2010 ~NA_character_,
    title == "Twin Peaks" & year(date) > 2010~NA_character_,
    TRUE ~title
  )) %>% filter(!is.na(title))

##Recreate economist graph
x %>% mutate(for_color = ifelse(title %in% three_year$title,title, "Other")) %>%
  ggplot(aes(x = date, y = av_rating)) + 
  geom_smooth(method = "lm",
              color = "gray30",
              se = FALSE,
              size = 0.5,
              alpha = 0.4,
              linetype = 2) +
  geom_smooth(aes(weight = share),
              color = "orange",
              method = "lm",
              size = 1, 
              se = FALSE, 
              alpha =0.4,
              linetype = 1
              ) + 
  geom_point(aes(size = share),
             alpha = 0.15,
              show.legend = FALSE,
             color = "gray30") + 
  geom_point(data= fix_df,
            aes(x = date,
                y = av_rating,
                color = title,
                size = share),
                alpha =1,
            show.legend = FALSE) + 
  geom_line(data= fix_df,
             aes(x = date, y = av_rating,
                 color = title),
            alpha = 0.8, size = 2,
            show.legend = FALSE) + 
  labs(x = "\nDate",
       y = "Average rating",
       color = "Show",
       title = "Average IMDB rating by year",
       subtitle = "Highlighted shows refer to shows with highest weighted rating in a five year period\nSome observations were removed",
       caption = "Tidy tuesday 2019 - Week 2\nTV Ratings") + 
  scale_size_continuous(range = c(1,12)) + 
  scale_color_viridis(option = "A",
                      discrete = TRUE,
                      begin = 0.3, end = 0.7) + 
  scale_x_date(breaks = seq.Date(ymd("1990-01-01"),ymd("2018-01-01"), by = "year"),
               labels = as.character(seq(1990, 2018, by = 1))) + 
  scale_y_continuous(limits = c(4,10)) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "gray95"),
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.1,linetype = 2,color = "gray80"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45),
    legend.position = "top"
  ) +
  guides(size = FALSE) + 
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5)) + 
  annotate("text",label = "Linear trend weighted by share",y = 8.6,
           x= ymd("2014-01-01"), angle = 3.5, fontface = "bold", color = "brown") + 
  annotate("text",label = "Linear trend without weights",y = 8.2,
           x= ymd("2014-01-01"), angle = 2, fontface = "bold", color = "black") + 
  annotate("text", label = "Twin Peaks", y=8.7187, x = ymd("1992-09-06"), size = 5,
           color = "#F76F5CFF", fontface = "bold") + 
  annotate("text", label = "The X-Files", y=8.7239, x = ymd("1998-02-04"), size = 5,
           color = "#DE4968FF", fontface = "bold") + 
  annotate("text", label = "Lost", y=8.6239, x = ymd("2004-05-06"), size = 5,
           color = "#B63679FF", fontface = "bold")  + 
  annotate("text", label = "Breaking Bad", y=9.8, x = ymd("2013-02-20"), size = 5,
           color = "#641A80FF", fontface = "bold") + 
  annotate("text", label = "Game of Thrones", y=8.9323, x = ymd("2015-05-13"), size = 5,
           color = "#641A80FF", fontface = "bold")

 
