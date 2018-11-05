remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_32/")
library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-06/us_wind.csv")
write_csv(x,"us_wind.csv")