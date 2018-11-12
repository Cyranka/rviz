rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/gg_animate_population/")
library(tidyverse);library(gganimate)

x <- read_csv("np2017_d1.csv")

##Select all population
y <- x %>% filter(SEX == 0 & ORIGIN ==0 & RACE ==0) %>%
  select(YEAR, POP_0:POP_100) %>%
  gather(age,total,-YEAR) %>%
  mutate(age = as.numeric(str_replace(age, "POP_",""))) %>%
  arrange(YEAR, age) %>% rename(year = YEAR)

y <- y %>% mutate(bracket = case_when(
  between(age,0,25) ~"0-25",
  between(age,18,64) ~"18~64",
  TRUE ~"Over 65"
))

age_plot <- y %>%
  ggplot(aes(x = age, y = total, fill = bracket, frame = year)) + geom_area(position = "identity") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 17),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15,face = "bold"),
    axis.title.y = element_text(size = 15,face = "bold"),
    plot.caption = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13, face = "bold")
  ) + 
  scale_x_continuous(
    breaks = c(seq(0,100, 5))
  )+ 
  scale_y_continuous(
    labels = prettyNum(seq(0,50000000,10000000),",")
  ) + 
  guides(fill = guide_legend(title = "Bracket",
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             keyheight = 0.5,
                             keywidth = 0.5)) + 
  labs(title = "Population distribution by age group",subtitle = 'Year: {frame_time}', x = '\nAge', y = 'Estimate') +
  transition_time(year) + ease_aes('linear')

animate(age_plot, nframes = 80, height = 600, width =1000)



##
y %>%
  ggplot(aes(x = age, y = total, fill = bracket, frame = year)) +
  geom_area(position = "identity") + 
  labs(title = "Population distribution by age group",subtitle = 'Year: {frame_time}', x = '\nAge', y = 'Estimate') + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 17, hjust = 0.5),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15,face = "bold"),
    axis.title.y = element_text(size = 15,face = "bold"),
    plot.caption = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13, face = "bold")
  ) + 
  scale_x_continuous(
    breaks = c(seq(0,100, 5))
  ) + 
  scale_y_continuous(
    labels = prettyNum(seq(0,50000000,10000000),",")
  ) + 
  guides(fill = guide_legend(title = "Bracket", title.position = "top",title.hjust = 0.5,label.position = "bottom"))
  