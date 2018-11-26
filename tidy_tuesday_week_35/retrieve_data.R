remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-27/baltimore_bridges.csv")

write_csv(x, "baltimore_bridges.csv")