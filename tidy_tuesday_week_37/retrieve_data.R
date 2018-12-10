rm(list =ls())

library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-11/nyc_restaurants.csv")

write_csv(x, "nyc_restaurants.csv")