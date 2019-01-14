rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv")

write_csv(x, "launches.csv")

##
y <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/agencies.csv")

write_csv(y, "agencies.csv")