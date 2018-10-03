remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_27/")
library(tidyverse);library(lubridate);library(viridis)

x <- read_csv("us_births_2000_2014.csv") %>%
  mutate(date = ymd(paste0(year, "-", month,"-",date_of_month)))

x %>% group_by(month, date_of_month) %>%
  summarise(mean_births = mean(births)) %>%
  mutate(month_name = month(month,label = TRUE, abbr = FALSE)) %>%
  ungroup() %>% mutate(row = row_number()) %>%
  ggplot(aes(x = row, y = mean_births, fill = as.character(month))) + 
  scale_fill_viridis(discrete = TRUE) + geom_area(show.legend = FALSE, size = 2)+ 
  theme_minimal() + 
  scale_x_continuous(expand = c(0,0)) + 
  theme(
    text = element_text(family = "Roboto"),
    axis.text.x = element_blank(),
    axis.title.x = element_text(hjust = 0.95, family = "Roboto", face = "bold"),
    axis.title.y = element_text(hjust =0.95, family = "Roboto", face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.spacing = unit(0,"cm"),
    strip.text = element_text(face = "bold")
  ) + facet_wrap(~month_name,scales = "free_x", nrow = 1) + 
  labs(x = "Day of the month",
       y = "Average number of births") + 
  scale_y_continuous(breaks = seq(0,15000,2500))
