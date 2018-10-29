remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_31/")
library(tidyverse)


x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r_downloads_year.csv")
write_csv(x, "r_downloads.csv")

y <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r-downloads.csv")
write_csv(x, "r_downloads_october_23_2018.csv")