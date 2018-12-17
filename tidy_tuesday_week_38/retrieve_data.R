remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_38/")

x <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-18/allCetaceanData.csv")


readr::write_csv(x,"all_cetacean_data.csv")