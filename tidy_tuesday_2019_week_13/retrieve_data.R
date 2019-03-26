remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_2019_week_13/")
library(tidyverse)

x<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")
write_csv(x, "seattle_pets.csv")