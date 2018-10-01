remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_27/")
library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-02/us_births_2000-2014.csv")

write_csv(x,"us_births_2000_2014.csv")