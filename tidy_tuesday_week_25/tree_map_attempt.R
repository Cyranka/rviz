remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_25/")
library(tidyverse)

x <- read_csv("us_airports.csv")

totals <- x %>% filter(year == 2016) %>%
  group_by(city) %>%
  summarise(total = sum(passengers)) %>%
  arrange(desc(total)) %>%
  mutate(prop = round(total/sum(total),3)*100) %>%
  mutate(city = str_replace_all(city, " International Airport",""))

library(treemap)
treemap(totals,
        index = "city",
        vColor = "prop",
        vSize = "prop",
        type = "value",
        #palette = c("dodgerblue3","darkorange","firebrick3","darkslategray3","darkgreen"),
        fontsize.labels =c(7,0),
        fontfamily.labels ="sans",
        fontface.labels = 2,
        force.print.labels = TRUE,
        border.lwds = 1)
