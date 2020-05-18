remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/francisco06121988/Desktop/rviz/538_midwest/")
library(tidyverse)

x <- read_rds("percentage_midwest.rds")

x %>%
    ggplot(aes(x= reorder(state,
                           percentage_in_midwest),
               y = percentage_in_midwest,
               label = scales::percent(percentage_in_midwest, accuracy = 1))) + 
    geom_col(fill = "firebrick1") + 
    geom_label(size =2.5,
               family = "Roboto Condensed",nudge_y = 0.03) + 
    coord_flip() + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0,0.9,by = 0.1),
                       limits = c(0,0.85)) + 
    theme(
        axis.ticks = element_blank(),
        text = element_text(family  = "Roboto Condensed"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10),
        panel.background = element_rect(fill = "gray95"),
        plot.background = element_rect(fill = "gray95"),
        axis.line.x = element_line(color= "gray20",size = 0.4),
        axis.line.y = element_line(color= "gray20",size = 0.4),
        legend.background = element_rect(fill = "gray95"),
        panel.grid = element_line(color = "white"),
        legend.position = "bottom"
    ) + 
    labs(title = '"Which states do you consider part of the Midwest?"',
         subtitle = "Percentage classifying each state as part of the Midwest, from a\nsurvey of 1,357 people identifying 'some' or 'a lot' as a Midwesterner",
         caption = "Original post:https://fivethirtyeight.com/features/what-states-are-in-the-midwest/\nSource: Survey Monkey/538",
         y = "",
         x = "State\n")