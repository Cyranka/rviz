remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/president_brazil/")
library(tidyverse);library(lubridate)

x <- readxl::read_excel("pres_brazil.xlsx") %>%
  mutate(date = ymd(date)) %>%
  mutate(candidate = factor(candidate, levels = c("Bolsonaro", "Haddad", "Ciro","Alckmin", "Marina")))

x %>% group_by(candidate) %>% arrange(candidate,date) %>%
  mutate(order = row_number(),
         factor_date = factor(date)) %>% 
  mutate(factor_date = str_replace(factor_date, "2018-","")) %>%
  ggplot(aes(x = factor_date, y = total, fill = candidate, label = total)) + geom_col(show.legend = FALSE, width = 0.9) + 
  facet_wrap(~candidate, nrow = 1,strip.position = "top") + theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    panel.spacing = unit(0,"cm"),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", colour = "white", family = "Roboto"),
    axis.text.x = element_text(angle = 90, margin = margin(t = -12),vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray40", color = "white"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 13)
  ) + 
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,by = 5)) + 
  scale_fill_manual(values = c("dodgerblue4", "firebrick", "orange3", "steelblue", "forestgreen")) +
  labs(x = "\nDate", y= "% of total votes",
       title = "Brazilian election: candidate support in major opinion polls",
       subtitle = "Minor candidates not shown",
       caption = "Ibope and Datafolha polls between August 20, 2018 and Ocotober 2, 2018")


