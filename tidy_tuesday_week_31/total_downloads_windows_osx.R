remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_31/")
library(tidyverse);library(viridis);library(lubridate);library(gridExtra)

x <- read_csv("r_downloads.csv")


##
x_2 <- x %>% group_by(date,os) %>% tally() %>%
  mutate(os = str_to_title(os)) %>%
  filter(!is.na(os))

##
month_levels <- c("Oct-2017","Nov-2017","Dec-2017",
                  "Jan-2018", "Feb-2018","Mar-2018",
                  "Apr-2018", "May-2018","Jun-2018",
                  "Jul-2018","Aug-2018","Sep-2018",
                  "Oct-2018")
osx <- filter(x_2, os == "Osx")


##Begin graph: osx
osx_graph <- osx %>% mutate(month = lubridate::month(date, label = TRUE),
               year = lubridate::year(date)) %>%
  mutate(month_year = factor(paste0(month, "-", year),levels = month_levels)) %>%
  filter(!month_year %in% c("Oct-2017")) %>%
  ggplot(aes(x = date, y = n,fill = month)) +
  geom_area(show.legend = FALSE,size = 0.5, col = "gray50") + 
  facet_wrap(~month_year,scales = "free_x", nrow = 1) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    axis.title.x = element_text(hjust = 0.5, family = "Roboto", face = "bold", size = 11),
    axis.title.y = element_text(hjust =0.5, family = "Roboto", face = "bold",size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.spacing = unit(-0.25,"cm"),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 10)
  ) + 
  labs(x = "Date",
       y = "Total downloads",
       title = "Total downloads by date by OSX users",
       subtitle = "October, 2017 was excluded\nDotted line represents the average for the period",
       caption = "Tidy tuesday week 31\nR and R package downloads") + 
  scale_fill_viridis(discrete = TRUE, option = "A",begin = 0.3, end = 0.8) + 
  scale_y_continuous(limits = c(0,2000),
                     breaks = seq(0,2000, by = 200),
                     labels = prettyNum(seq(0,2000, by = 200),big.mark = ",")) + ##Average is 502
  geom_hline(yintercept = 539, size = 0.25, linetype = 2) 
  
##Windows graph
win <- filter(x_2, os == "Win")

win_graph <- win %>% mutate(month = lubridate::month(date, label = TRUE),
                            year = lubridate::year(date)) %>%
  mutate(month_year = factor(paste0(month, "-", year),levels = month_levels)) %>%
  filter(!month_year %in% c("Oct-2017")) %>%
  ggplot(aes(x = date, y = n,fill = month)) +
  geom_area(show.legend = FALSE,size = 0.5, col = "gray50") + 
  facet_wrap(~month_year,scales = "free_x", nrow = 1) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    axis.title.x = element_text(hjust = 0.5, family = "Roboto", face = "bold", size = 11),
    axis.title.y = element_text(hjust =0.5, family = "Roboto", face = "bold",size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.spacing = unit(-0.25,"cm"),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 10)
  ) + 
  labs(x = "Date",
       y = "Total downloads",
       title = "Total downloads by date by Windows users",
       subtitle = "October, 2017 was excluded\nDotted line represents the average for the period",
       caption = "Tidy tuesday week 31\nR and R package downloads") + 
  scale_fill_viridis(discrete = TRUE, option = "D",begin = 0.5) + 
  scale_y_continuous(limits = c(0,5000),
                     breaks = seq(0,5000, by = 500),
                     labels = prettyNum(seq(0,5000, by = 500),big.mark = ",")) + ##Average is 1800
  geom_hline(yintercept = 1900, size = 0.25, linetype = 2) 


##
grid.arrange(win_graph, osx_graph, ncol=1)
