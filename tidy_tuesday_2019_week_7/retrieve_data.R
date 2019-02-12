remove(list = ls())
library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

write_csv(x, "climate_spending.csv")