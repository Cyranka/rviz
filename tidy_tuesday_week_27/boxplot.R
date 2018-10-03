remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_27/")
library(tidyverse);library(lubridate)

x <- read_csv("us_births_2000_2014.csv") %>%
  mutate(date = ymd(paste0(year, "-", month,"-",date_of_month)))


averages_days <- x %>% group_by(year, month) %>%
  mutate(percent = round(births/sum(births)*100,2)) %>%
  group_by(date_of_month) %>%
  summarise(avg = mean(percent))


l <- x %>% group_by(year, month) %>%
  mutate(percent = round(births/sum(births)*100,2)) %>%
  mutate(date = ymd(paste0(year, "-", month,"-",date_of_month)))


l %>% ggplot(aes(x = date_of_month, y = percent, group = date_of_month)) + 
  geom_boxplot() + theme_minimal() + 
  scale_x_continuous(breaks = c(1:31)) + 
  labs(x = "Day of the month", y = "% of births",
       title = "Distribution of the proportion of monthly births for each day",
       subtitle = "There is a visible decline on the 13 compared to neighbouring days",
       caption = "Tidy Tuesday Week 27 - Births in the United States 2000-2014\nSource:538") + 
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size =12, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust =0.5),
    plot.caption = element_text(size =8)
  )