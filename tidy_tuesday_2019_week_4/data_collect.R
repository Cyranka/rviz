remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/prison_population.csv")
write_csv(x,"prison_population.csv")


##
y <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/incarceration_trends.csv")
write_csv(y, "incarceration_trends.csv")