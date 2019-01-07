remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

if(("tidy_tuesday_2019_week_2" %in% list.files()) == FALSE){
  dir.create("tidy_tuesday_2019_week_2")
}else{
  print("Directory already created")
}

##
setwd("tidy_tuesday_2019_week_2/")
x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

write_csv(x, "IMDb_Economist_tv_ratings.csv")