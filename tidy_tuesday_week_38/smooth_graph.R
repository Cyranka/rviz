remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_38/")
library(tidyverse);library(viridis);library(ggridges);library(lubridate)

x <- read_csv("all_cetacean_data.csv")


##
died <- x %>% filter(acquisition %in% c("Born", "Capture") & status == "Died") %>%
  mutate(origin = year(originDate),
         death = year(statusDate)) %>%
  filter(!is.na(death)) %>%
  mutate(life_time = death - origin) %>%
  mutate(decade = origin - origin %%10) %>%
  filter(decade >= 1960) %>% filter(life_time >0 )


year_changes <- died %>% group_by(origin) %>%
  summarise(med_point = median(life_time))

died %>%
  ggplot(aes(x = origin, y = life_time, color = acquisition)) + 
  geom_smooth(
              se = FALSE,
              size = 3,
              method = "lm", formula = y~poly(x,5),
              show.legend = FALSE) + 
  geom_jitter(aes(x  = origin, y = life_time, fill = factor(acquisition)),
             color = "white",
             data = died,
             inherit.aes = FALSE,
             size =2, alpha = 0.4, shape = 21, show.legend = FALSE) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    axis.line.x = element_line(size =1.5,color = "gray25"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size =0.15, linetype = 2),
    panel.grid.major.y = element_line(size =0.15, linetype = 2),
    plot.title = element_text(size = 15, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 13, face = "bold",color = "white"),
    plot.caption = element_text(size = 10,color = "white"),
    strip.text = element_text(size = 11,color = "white"),
    axis.text = element_text(size = 11,color = "white"),
    axis.title = element_text(size = 12,color = "white"),
    plot.background = element_rect(fill = "black",color = "white"),
    strip.background = element_rect(fill = "gray25")
  ) + 
  lemon::facet_rep_wrap(~factor(acquisition, levels = c("Capture", "Born"),labels = c("Captured", "Born")), nrow = 2,repeat.tick.labels = TRUE) + 
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.4,end = 0.9) + 
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.4, end = 0.9) + 
  labs(x = "\nYear born/captured", 
       y = "Median life span",
       title = "Median life span of cetaceans captured/born each year",
       subtitle = "Records before 1960 not shown",
       caption = "Tidy tuesday week 38\nCetacean data") + 
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, title = "Acquisition"),
         fill = guide_legend(title.position = "top", title.hjust = 0.5))  +
  scale_x_continuous(breaks = seq(1960,2015, by = 5))
  