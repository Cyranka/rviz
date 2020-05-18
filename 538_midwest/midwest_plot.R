remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/francisco06121988/Desktop/rviz/538_midwest/")
library(tidyverse);library(urbnmapr);library(ggforce)

x <- read_rds("percentage_midwest.rds")

par(mar=c(0,0,1,0))

urbnmapr::states %>%
    filter(!state_abbv %in% c("HI","AK")) %>%
    left_join(x, by = c("state_name" = "state")) %>%
    ggplot(aes(x = long, y = lat, group = group,fill = percentage_in_midwest)) + 
    geom_polygon(color = "gray30",
                 size  = 0.2) + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    scale_fill_gradient(na.value = "gray90",
                        low = "seashell1",high = "firebrick1",
                        labels = scales::percent,
                        breaks = seq(0.1,0.8,by = 0.1)) + 
    theme(
        text = element_text(family  = "Roboto Condensed"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.background = element_rect(fill = "gray95"),
        plot.background = element_rect(fill = "gray95"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        panel.grid = element_line(color = "white"),
        legend.position = "bottom"
    ) +
    labs(title = '"Which states do you consider part of the Midwest?"',
         subtitle = "Percentage classifying each state as part of the Midwest, from a\nsurvey of 1,357 people identifying 'some' or 'a lot' as a Midwesterner",
         caption = "Original post:https://fivethirtyeight.com/features/what-states-are-in-the-midwest/\nSource: Survey Monkey/538") + 
    guides(fill = guide_colorbar(title.position = "top",
                                 barwidth = 20,
                                 barheight = 0.8,
                                 title = "")) + 
    geom_text(aes(x = long, y = lat, label = state_abbv),
               data = urbnmapr::get_urbn_labels() %>% 
                  semi_join(x, by = c("state_name" = "state")),
               inherit.aes = FALSE,
              family = "Roboto Condensed",
              fontface = "bold",
              size = 3.5) + 
    geom_mark_circle(aes(x = -107.000,y = 43.9999,
                         label = "Wyoming",
                         description = "10% of respondents considered Wyoming as part of the Midwest"),
                     inherit.aes = FALSE,
                     label.fontsize = 6,
                     con.type = "elbow",
                     label.buffer = unit(2,"cm"),
                     expand = unit(0,"mm")) + 
    geom_mark_circle(aes(x = -89.190,y = 38.5417,
                         label = "Illinois",
                         description = "81% of respondents considered Illinois as part of the Midwest"),
                     inherit.aes = FALSE,
                     label.fontsize = 6,
                     con.type = "elbow",
                     expand = unit(0,"mm"),label.buffer = unit(1,"mm"))

