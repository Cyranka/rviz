remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_2019_week_15/")
library(tidyverse);library(lubridate)

brexit <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv") %>%
    mutate(date = lubridate::dmy(date),
           month = floor_date(date, unit = "month"))

brexit %>%
    group_by(month) %>%
    summarise(mean_right = mean(percent_responding_right),
           mean_wrong = mean(percent_responding_wrong)) %>%
    mutate(result = mean_wrong-mean_right,
           my_fill = result >0) %>%
    ggplot(aes(x = month, y = result, fill = my_fill)) + 
    labs(x = "\nMonth",
         y = "Difference",
         title = "In hindsight, do you think the UK was right or wrong to leave the EU?",
         subtitle = "Difference between the average proportion opposing it and the average proportion in favor of it\nData aggregated by month",
         caption = "Source: NatCen Social Research\nTidy Tuesday 2019, Week 16") +
    geom_col() + 
    scale_x_date(date_labels = "%b-%Y",
                 breaks = seq.Date(ymd("2016-08-01"),ymd("2018-09-01"), by = "month")) + 
    theme_minimal() + 
    theme(
        text = element_text(family = "Roboto Condensed"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.3),
        plot.background = element_rect(fill = "gray85"),
        plot.title = element_text(face = "bold")
    ) + 
    scale_y_continuous(limits = c(-7,7),
                       breaks = seq(-7,7, by = 1)) + 
    scale_fill_manual(values = c("firebrick3",
                                 "darkblue")) + 
    annotate(geom = "text",
             x = ymd("2017-01-01"),
             y = -5.5,
             label = "Values in this region show stronger support for Brexit",
             size = 2.5,
             fontface = "bold") + 
    annotate(geom = "text",
             x = ymd("2018-03-01"),
             y = 6.5,
             label = "Values in this region show stronger support for remaining in the EU",
             size = 2.5,
             fontface = "bold")

