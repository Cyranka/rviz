remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/francisco06121988/Desktop/rviz/538_midwest/")
library(tidyverse);library(urbnmapr);library(ggforce)

x <- read_rds("percentage_south.rds")

par(mar=c(0,0,1,0))

urbnmapr::states %>%
    filter(!state_abbv %in% c("HI","AK")) %>%
    left_join(x, by = c("state_name" = "state")) %>%
    ggplot(aes(x = long, y = lat, group = group,fill = percentage_in_south)) + 
    geom_polygon(color = "gray30",
                 size  = 0.2) + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    scale_fill_gradient(na.value = "gray90",
                        low = "seashell1",high = "deepskyblue2",
                        labels = scales::percent,
                        breaks = seq(0.1,0.9,by = 0.1)) + 
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
    labs(title = '"Which states do you consider part of the South?"',
         subtitle = "Percentage classifying each state as part of the South, from a\nsurvey of 1,135 people identifying 'some' or 'a lot' as a Southerner",
         caption = "Original post:https://fivethirtyeight.com/features/which-states-are-in-the-south/\nSource: Survey Monkey/538") + 
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
    geom_mark_circle(aes(x = -105.5478,y = 38.9972,
                         label = "Colorado",
                         description = "1% of respondents considered Colorado as part of the South"),
                     inherit.aes = FALSE,
                     label.fontsize = 6,
                     con.type = "elbow",
                     label.buffer = unit(0,"cm"),
                     expand = unit(0,"mm")) + 
    geom_mark_circle(aes(x = -83.4426,y = 32.6415,
                         label = "Georgia",
                         description = "89% of respondents considered Georgia as part of the South"),
                     inherit.aes = FALSE,
                     label.fontsize = 6,
                     con.type = "elbow",
                     label.buffer = unit(0,"cm"),
                     expand = unit(0,"mm"))