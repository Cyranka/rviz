remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext)
csv_files <- grep("\\.csv",list.files(), value = TRUE)

set.seed(4)
x <- bind_rows(lapply(csv_files, function(i) read_csv(i) %>% sample_n(30000)))
write_csv(x, "sampled_tweets.csv")