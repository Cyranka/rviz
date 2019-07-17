remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse);library(lubridate)

r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

current_year <- r4ds_members %>%
    filter(between(date,ymd("2017-09-01"),ymd("2019-06-30")))

labels <- c(`2017-09-01` = "September, 2017",
            `2017-10-01` = "October, 2017",
            `2017-11-01` = "November, 2017",
            `2017-12-01` = "December, 2017",
            `2018-01-01` = "January, 2018",
            `2018-02-01` = "February, 2018",
            `2018-03-01` = "March, 2018",
            `2018-04-01` = "April, 2018",
            `2018-05-01` = "May, 2018",
            `2018-06-01` = "June, 2018",
            `2018-07-01` = "July, 2018",
            `2018-08-01` = "August, 2018",
            `2018-09-01` = "September, 2018",
            `2018-10-01` = "October, 2018",
            `2018-11-01` = "November, 2018",
            `2018-12-01` = "December, 2018",
            `2019-01-01` = "January, 2019",
            `2019-02-01` = "February, 2019",
            `2019-03-01` = "March, 2019",
            `2019-04-01` = "April, 2019",
            `2019-05-01` = "May, 2019",
            `2019-06-01` = "June, 2019")

my_colors <- LaCroixColoR::lacroix_palette("PeachPear", n = 30, type = "continuous")[1:25]


current_year %>%
    mutate(month = floor_date(date,"month")) %>%
    group_by(month) %>% mutate(obs_n = row_number()) %>% 
    ggplot(aes(x = obs_n, y = daily_active_members,
               color = as.character(month),fill = as.character(month))) + 
    geom_area(show.legend = FALSE,alpha = 0.8) + 
    geom_line(show.legend = FALSE) + 
    #geom_point(show.legend = FALSE, size = 0.5) + 
    facet_wrap(~month,
               labeller = as_labeller(labels),
               nrow = 4) + 
    hrbrthemes::theme_ft_rc(base_size = 10,
                            base_family = "Arial Narrow",
                            axis_text_size = 9,
                            axis_title_size = 12,
                            axis_title_face = "bold",
                            strip_text_size = 12,
                            strip_text_face = "bold") + 
    labs(y = "Total members\n",
         x = "Day",
         title = "Daily active members of the R4DS online learning community",
         subtitle = "Plot only includes months with complete data") + 
    theme(
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(-0.9,"cm"),
        axis.text.x = element_blank()
    ) +
    scale_x_continuous(breaks = c(1,5,10,15,20,25,30)) +
    scale_y_continuous(breaks = seq(0,250,by = 50),
                       limits = c(0,275)) + 
    scale_fill_manual(values = my_colors) + 
    scale_color_manual(values = my_colors)