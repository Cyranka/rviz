remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-13/malaria_inc.csv")
write_csv(x,"malaria_inc.csv")

y <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018-11-13/malaria_deaths.csv")
write_csv(x,"malaria_deaths.csv")

z <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-13/malaria_deaths_age.csv")
write_csv(z,"malaria_deaths_age.csv")