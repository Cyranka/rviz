rm(list =ls())

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-04/fastfood_calories.csv")
write_csv(x %>% select(-X1), "fastfood_calories.csv")
