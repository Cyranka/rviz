remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_24/")
library(tidyverse)
library(dendextend)

x <- read_csv("cats_vs_dogs.csv") %>%
  mutate(state = ifelse(state == "District of Columbia", "DC", state))

y <- readxl::read_excel("results_2016.xlsx") %>%
  mutate(diff = Trump - Clinton) %>%
  magrittr::set_colnames(c("state","clinton", "trump","diff"))



z <- x %>% inner_join(y) %>%
  filter(state != "DC") %>%
  mutate(dog_cat_ratio = dog_population/cat_population)

z %>%  ggplot(aes(x = trump, y = dog_cat_ratio, size = dog_cat_ratio)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method = "lm", se = FALSE) + theme_minimal()

summary(lm(dog_cat_ratio~trump, data = z))