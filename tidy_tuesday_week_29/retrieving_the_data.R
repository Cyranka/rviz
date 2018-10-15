remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_29/")
x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-16/recent-grads.csv")

write_csv(x, "recent_grads.csv")